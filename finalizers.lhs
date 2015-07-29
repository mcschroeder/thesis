%include thesis.fmt

\chapter{Transactional Memory with Finalizers}
\label{chap:finalizers}

\section{STM and ACID}
\label{sec:stm-and-acid}

Atomicity, consistency, isolation and durability (ACID) are the cornerstones of any database system.
Haskell's STM gives us three of these properties:
transactions are always \emph{atomic} and have an \emph{isolated} view of memory;
dynamically-checked invariants can be used to ensure that the state of the system is \emph{consistent}.
The one missing property is \emph{durability}.

Making data durable means serializing it to storage.
This is of course an I/O action, which is not allowed within the confines of STM.
For good reason: STM transactions can retry at any time and I/O actions are in general neither idempotent nor interruptible.
However, notice that for the purposes of durability it is not necessary to run arbitrary I/O at arbitrary moments during a transaction.
We merely need to run a single I/O action to serialize our data at the end of the transaction.

The naive approach is to first atomically perform some STM computation and then independently serialize its result:
\begin{code}
durably :: STM a -> IO ()
durably m = do  x <- atomically m
                serialize x
\end{code}
There are two issues with this.
First, by virtue of the serialization happening independently of the atomic block, after we have performed a transaction another thread could perform a second transaction and serialize it before we have finished serializing the first one.
Depending on our serialization method, we could end up with an inconsistent state between the memory of our program and what is stored on disk.
At the very least, there is no guarantee that the ordering of events is preserved.
Secondly, the function |serialize| might not terminate at all;
it could throw an exception or its thread could die.
Again we would end up with an inconsistent state and possibly data loss.

So the serialization needs to be somehow coupled with the atomic transaction in such a way that the transaction only commits \emph{after} the data has been serialized.
We might try to use the ominously named |unsafeIOToSTM| function:
\begin{code}
durably :: STM a -> IO ()
durably m = atomically $ do  x <- m
                             unsafeIOToSTM (serialize x)
\end{code}
The type of |unsafeIOToSTM| is |IO a -> STM a|, effectively allowing us to circumvent the type system so we can unsafely perform I/O in the STM monad.
But this too must fail:
even if it is the last statement in the atomic block, |serialize| is performed before the transaction commits.
The transaction might yet have to retry because of a conflict with another transaction.
In doing so it also executes |serialize| again.
Unless |serialize| is idempotent, this is certainly undesirable.
Furthermore, if the thread receives an asynchronous exception, the transaction will abort in an orderly fashion, while |serialize|, with its irrevocable side effects, cannot be undone.

This leads us to the realization that \emph{the transaction must only commit after the data has been serialized, and the data must only be serialized after the transaction has committed}.
We can escape the paradox by clarifying the second restriction:
the data can in fact already be serialized after the transaction is merely \emph{guaranteed} to commit.
Meaning: at the point of serialization, the transaction has not yet made its effects visible to other transactions, but there are no conflicts preventing it from doing so.
And until serialization is finished there must be no way for any such conflicts to arise;
for all intents and purposes, the transaction must count as committed, even though its effects are not visible yet and it could still roll back by its own volition (but not due to conflicts with other transactions).

What is needed, then, is a new STM primitive.
Not a combination of existing functions, but a new fundamental operation that does exactly what we want.
It would have to be implemented directly in the runtime system and we would have to make sure that it is sound and does not allow us to undermine the safety of STM in general.

\medskip

I will now describe this primitive, give its detailed semantics and talk about my implementation of it in the Glasgow Haskell Compiler.

%============================================================

\clearpage

\section{Finalizers}
\label{sec:stm-overview}

The main idea is to introduce a new function
\begin{code}
atomicallyWithIO :: STM a -> (a -> IO b) -> IO b
\end{code}
%atomicallyWithIO_ :: STM a -> IO () -> IO a

Like the existing |atomically| operation, |atomicallyWithIO| transforms a computation of type |STM a| into an I/O action.
Additionally, it takes a \emph{finalizer}, which is a function of type |a -> IO b|, viz.\ an I/O action that can depend on the result of the STM computation, and combines it with the transaction in such a way that:

\begin{enumerate}
\item The finalizer is only performed if the STM transaction is guaranteed to commit.
\item The STM transaction only commits (i.e.\ makes its effects visible to other transactions) if the finalizer finishes without raising an exception.
\end{enumerate}

A detailed specification of |atomicallyWithIO| will be given in \Cref{sec:stm-fin-semantics}.
Everything described there has been fully implemented by me in GHC and I will discuss that implementation in \Cref{sec:stm-implementation}.
But first let us look at some motivating examples, which will demonstrate the usefulness of this new operation and highlight some of its finer issues.

%------------------------------------------------------------

\subsection{Example 1: Printing tickets}

Say we want to sell tickets for an event.
We have a set number of tickets, stored in a transactional variable |tickets| of type |TVar Int|, and the following function to get the next available ticket:
\begin{code}
nextTicket :: STM Int
nextTicket = do  t <- readTVar tickets
                 when (t == 0) (error "sold out")
                 writeTVar tickets (t-1)
                 return t
\end{code}
Our box offices can then use a statement like |t <- atomically nextTicket| to safely acquire a ticket.

However, selling tickets involves actually printing the ticket number on a piece of paper.
What happens if the printer malfunctions, or runs out of paper?
If for some reason the ticket cannot be printed, we do not want that ticket number to be lost forever; it has not yet been sold after all.
The solution, of course, is to use a finalizer:
\begin{code}
printTicket :: Int -> IO ()
printTicket t = ...

sellTicket :: IO ()
sellTicket = atomicallyWithIO nextTicket printTicket
\end{code}
If |printTicket| throws an exception, the transaction rolls back and the |tickets| counter was never decreased.

%------------------------------------------------------------

\subsection{Example 2: User input}

An I/O action can of course collect input as well.
Consider the following example of an ATM.
The function to withdraw money from an account is quite standard; as is, one would imagine, the function to dispense actual cash from the machine:

%{
%format acc = "\Varid{acc}"
%format amount = "\Varid{amount}"
%format bal = "\Varid{bal}"
\begin{code}
type Account = TVar Int
withdraw :: Account -> Int -> STM ()
withdraw acc amount = do  bal <- readTVar acc
                          if bal < amount
                              then error "insufficient funds"
                              else writeTVar acc (bal-amount)

dispenseCash :: Int -> IO ()
dispenseCash amount = ...
\end{code}
But instead of simply stringing these two functions together, we use a finalizer to ask the user for final confirmation before actually going through with the transaction:
%format answer = "\Varid{answer}"
\begin{code}
getMoney :: Account -> Int -> IO ()
getMoney acc amount = atomicallyWithIO action finalizer
  where
    action = do  withdraw acc amount
                 bal <- readTVar acc
                 return bal
    finalizer bal = do  putStrLn ("New balance: " ++ show bal)
                        putStrLn "Confirm (yes/no)?"
                        answer <- getLine
                        case answer of
                            "yes"  -> dispenseCash amount
                            _      -> error "cancelled"
\end{code}
%}

What this example shows is how we can use |atomicallyWithIO| to play 
out the revocable effects of a transaction (e.g.\ withdrawing money from an account) and use the results in a side-effecting manner (e.g.\ showing the user the new balance of her account and asking for confirmation) before committing to those effects.

Note that the balance we show to the user will be the final balance.
There is no way another transaction could change it before we finish the I/O action.
Once the I/O phase begins, the transaction is locked. 
At this point, only the finalizer itself can cause a rollback.
And only when the finalizer finishes by returning is the transaction made visible to the rest of the world.

% performance bottleneck
There are obvious performance implications with this scheme:
if the I/O action takes a long time, other transactions will have to wait before they can make progress.
This bottleneck is unavoidable, the transition from STM to I/O naturally creates a serialization point.
It is up to the programmer to not waste undue time or otherwise ensure that the finalizer finishes in a timely manner, e.g.\ by using the standard |timeout| function.

%------------------------------------------------------------

\subsection{Example 3: Nesting}
\label{subsec:stm-example-nesting}

Since the finalizer can be an arbitrary I/O action, the question arises:
can we run |atomicallyWithIO| \emph{inside} |atomicallyWithIO|?

Running |atomically| inside |atomically| is evidently not possible --- the type system forbids it.
Even something like \begin{code}atomically (unsafeIOToSTM (atomically m))\end{code} will not work: GHC detects such nefariousness and throws a runtime exception.
The runtime system does not support running a transaction inside another transaction, because there is no single intuitive way to resolve problems such as conflicting transactions or nested rollback.
Luckily, it does not appear that this kind of nesting is necessary in practice or even useful.

But running |atomically| within a finalizer is a different story.
First of all, this is eminently useful.
Consider the following example, in which a counter is atomically increased only after confirmation has been received from two different servers.
For efficiency, both servers are queried in parallel:
%{
%format counter = "\Varid{counter}"
%format server = "\Varid{server}"
%format server1
%format server2
%format r1
%format r2
\begin{code}
atomicallyWithIO  (do  n <- readTVar counter
                       writeTVar x (n+1)
                       return n
                  )
                  (\n -> do  (r1,r2) <- concurrently  (post n server1)
                                                      (post n server2)
                             if r1 && r2 
                                 then  return ()
                                 else  error "rejected"
                  )
\end{code}
%}
The function |concurrently :: IO a -> IO b -> IO (a,b)| is exported by the \package{async} library \parencite{marlow-2013-async}, which provides \textquote{a set of operations for running IO operations asynchronously and waiting for their results}.
The library is implemented using STM.
When executing |concurrently|, there are actually multiple atomic transactions happening behind the scenes.

Many complex I/O actions might use STM internally.\footnote{
Indeed, as of GHC 7.8, the base library itself makes use of STM in \texttt{GHC.Event.Unique}.
This module is in turn used by I/O primitives like |threadDelay|.
Thus STM usage is actually spread throughout large parts of the I/O subsystem.
}
There is no reason why these should not also be usable within the finalizer of |atomicallyWithIO|.
The type system allows it, and the semantics are clear as well:
finalizers are already regarded as irrevocable.
A transaction run within the finalizer of another transaction is completely independent from that other transaction.
There is only one restriction:
\emph{the inner transaction cannot modify any of the transactional variables used by the outer transaction};
this would inevitably lead to a deadlock, as the inner transaction would have to wait for the outer transaction to commit and release its locks on the shared variables, while the outer transaction can only commit once the inner transaction has committed.

But are these kinds of deadlock situations not exactly what STM is supposed to protect us from?
True, but I feel that introducing the possibility of deadlocks --- limited to this one specific scenario, in which the nested transactions operate on the same set of variables, which I believe is rather artificial --- is justified by the great usefulness of allowing independent STM transactions to run during an STM finalizer.
Also, the runtime system can always detect these kinds of deadlocks instead of simply looping forever, and it will throw an exception that gracefully aborts the transactions involved and that could be used to pinpoint the source of the bug.

In principle, circular dependencies between shared variables could even be detected statically, at compile time.
Trying to enforce the non-circularity via the type system would probably involve advanced type system features implemented by GHC extensions, if not require dependent types outright.
In any case, it would necessitate major backwards-incompatible changes to the existing STM API, which is something I wanted to avoid.
Alternatively, one could imagine a separate pre-processing step in the compiler, but this seems rather inelegant and more trouble than it is worth.

We should note an important point here:
a finalizer has the same global view of the world as any other I/O action running at the same time.
In particular, the new value of a |TVar| updated during the STM part of the transaction will not be visible in the finalizer.
The variable can be read safely -- a deadlock only happens if an inner transaction tries to \emph{modify} a shared variable -- but the value returned will be its old value, which is the actual, globally known value of the |TVar|, at this moment in time.

%============================================================



\section{Related Work}
\label{sec:finalizers-related-work}

% onCommit
Extending Haskell's STM to allow safe combination of atomic blocks with I/O actions has been proposed from the very beginning.
In a 2006 post on the \texttt{haskell-cafe} mailing list, Simon Peyton Jones suggested an operator
\begin{code}
 onCommit :: IO a -> STM ()
\end{code}
that \textquote{would queue up an IO action to be performed when the transaction commits}.\footnote{\url{http://www.haskell.org/pipermail/haskell-cafe/2006-November/019771.html}}
The \package{stm-io-hooks} package \parencite{robinson-kuklewicz-2012} implements this operator in a custom STM monad that is meant as drop-in replacement for the system one.
|onCommit| has the same semantics as |atomicallyWithIO|: \textquote{The commit IO action will be executed iff the transaction commits.}
The difference is that |onCommit| is truly composable.
Any STM function can use |onCommit| to add an I/O action to the list of actions to be executed when the atomic block commits.
The caller of |atomically| does not have to be aware of it.

While greater composability seems to be more in the spirit of Haskell's STM, I think that in this case it is actually dangerous.
I/O is fundamentally not composable in the same way that STM is.
Consider the following scenario:

\begin{code}
foo  :: STM ()
foo  = do  writeTVar a 1
           onCommit (serialize "a" 1)

bar  :: STM ()
bar  = onCommit (error "rollback")

baz  :: IO ()
baz  = atomically (foo >> bar)
\end{code}
Calling |baz| will lead to an inconsistent state:
the |onCommit| action of |bar| will abort the whole transaction and so the STM effects of |foo| are rolled back, yet \emph{the I/O effects are not}.
Even worse, there is no way from looking only at |baz| to discern that any of this is going on.
The composability of |onCommit| hides effects that in my opinion should be made explicit.
This may not be an issue in all cases, but for serialization it clearly is.
Using |atomicallyWithIO| is safer in this regard, at the cost of reduced composability.

%============================================================

%TODO: re-check all the semantics, just to be safe
\input{finalizers-semantics}

%============================================================

\section{Implementation}
\label{sec:stm-implementation}

The changes necessary to add finalizers to GHC's STM implementation are surprisingly few.
Before describing them I will once again first give a brief overview of the status quo.
To avoid getting bogged down in minutiae, my description of the existing implementation will be a careful simplification, focusing only on the relevant parts of the system.
The interested reader is referred to the STM Commentary \parencite{yates-2013} for a more thorough description, and to the GHC source code itself for all the gory details.\footnote{\url{http://git.haskell.org/ghc.git}}
A fork of GHC containing the changes described in this section is available at \url{http://github.com/mcschroeder/ghc}.

%------------------------------------------------------------

\subsection{Original STM interface}

% Original STM interface
When evaluating the |atomically| function, the runtime system pushes an \rts{ATOMICALLY{\_}FRAME} onto the execution stack of the current thread.
It then calls \rts{stmStartTransaction} to initialize a new thread-local \emph{transactional record} (\rts{TRec}).
%
% TRec
During the transaction, all read and write operations are recorded in the \rts{TRec}.
Every \rts{TVar} accessed by the transaction has an entry in the record.
Each entry contains a reference to the value of the \rts{TVar} at the start of the transaction (\rts{expected{\_}value}) and a reference to the new value the \rts{TVar} will have when the transaction commits (\rts{new{\_}value}).
If a \rts{TVar} has only been read during a transaction, these two values will be identical.
The \rts{TVar}s themselves are not modified until the transaction commits.
% and are only read directly once in each transaction.
%All subsequent reads happen on the \rts{TRec}.

% validation and commit
When execution returns to the \rts{ATOMICALLY{\_}FRAME}, the commit is initiated by calling \rts{stmCommitTransaction}.
First, the \rts{TRec} is \emph{validated} by checking if the \rts{expected{\_}value} of each entry is pointer-equal to the \rts{current{\_}value} field of the corresponding \rts{TVar}, i.e.\ if the \rts{TVar} has changed in between the transaction starting and committing.
If everything is as expected, the \rts{TVar} is locked by setting \rts{current{\_}value} to point to the committing \rts{TRec}.
If there is a mismatch, i.e.\ the \rts{TVar} was modified by someone else, the \rts{TRec} is discarded and the transaction restarted.
When validation is successful for the whole \rts{TRec}, the \rts{TVar}s are one by one updated by setting their \rts{current{\_}value} to the \rts{new{\_}value} of the corresponding \rts{TRec} entry, which also unlocks them again.

% atomicity
Because all \rts{TVar}s are locked during validation and each one is only unlocked after it has been updated, the whole commit happens atomically with respect to other transactions.
Any other transaction trying to commit at the same time will fail its validation.
A transaction trying to read a locked \rts{TVar} will briefly block, but since \rts{TVar}s are only ever locked for very short periods of time, this is not a problem.

%------------------------------------------------------------

\begin{figure}
\begin{Verbatim}[frame=single,framesep=1em,commandchars=\\\{\}]
// Basic transaction execution
TRec *stmStartTransaction();
Closure *stmReadTVar(TRec *trec, TVar *tvar);
void stmWriteTVar(TRec *trec, TVar *tvar, Closure *new_value);

// Transaction commit operations
\colorA{Bool stmPrepareToCommitTransaction(TRec *trec);}
\colorA{void} stmCommitTransaction(TRec *trec);

// Blocking operations
Bool stmWait(TRec *trec);
Bool stmReWait(TRec *trec);

// Transactional variable
struct TVar \{
  Closure    *current_value;
  \colorA{TRec       *frozen_by;}
\}

// Transactional record entry
struct TRecEntry \{
  TVar        *tvar;
  Closure     *expected_value;
  Closure     *new_value
  TRecEntry   *next_entry;
\}

// Transactional record
struct TRec \{
  TRec         *enclosing_trec;
  TRecEntry    *next_entry;
\}
\end{Verbatim}
\caption{GHC's STM runtime interface (simplified), extended to support \colorA{finalizers}}
\end{figure}

%------------------------------------------------------------

\subsection{Adding |atomicallyWithIO|}

The finalizer given to |atomicallyWithIO| should be executed after it is guaranteed that the transaction can no longer abort due to conflicts with other transactions, but before the transaction's memory effects are made visible.
This is only possible at one moment:
during \rts{stmCommitTransaction}, between the successful validation of the \rts{TRec} and the updating of the \rts{TVar}s, while all the \rts{TVar}s are still locked.

To achieve this, the validation part of \rts{stmCommitTransaction} has been split off into the new function \rts{stmPrepareToCommitTransaction}.
Committing is now a three-part sequence:
first, \rts{stmPrepareToCommitTransaction} is called to validate the transactions;
if successful, a new \rts{ATOMICALLY{\_}FRAME} is pushed onto the stack and execution jumps to the finalizer code;
when the finalizer is done and execution has returned to the \rts{ATOMICALLY{\_}FRAME}, \rts{stmCommitTransaction} is called to finish the commit and update the \rts{TVar}s.

% lock problems
There is one problem: if the \rts{TVar}s are locked while the finalizer is running, then any transaction attempting to access any of those locked \rts{TVar}s will immediately block until the finalizer is finished.
This is obviously bad for performance, especially considering that the blocking occurs even when both transactions are read-only.
Giving up on read-parallelism is clearly unacceptable.
But not locking the \rts{TVar}s while the finalizer is running is also not possible: the transaction could then unknowingly become invalidated by the actions of another transaction and still commit, resulting in an inconsistent state.

% freezing
The solution: we \emph{freeze} the \rts{TVar}s during finalization.
This puts them into a kind of in-between state, where they can still be read by other transactions, but any transaction that attempts to commit an update to a frozen \rts{TVar} will block until the \rts{TVar} is unfrozen again.
The new \rts{TVar} field \rts{frozen{\_}by} is set during \rts{stmPrepareToCommitTransaction}, after all \rts{TVar}s have been successfully validated, and points to the finalizing \rts{TRec}; 
\rts{frozen{\_}by} is cleared again during \rts{stmCommitTransaction}.
Although the \rts{TVar}s need to be locked briefly when modifying their \rts{frozen{\_}by} fields (or indeed any other of their fields), they no longer need to be locked for the whole duration of the finalizer.

Unfortunately, it is necessary to lock \emph{all} \rts{TVar}s that are involved in a transaction, \emph{if} the transaction does indeed have a finalizer.
Usually a read-phase is used, meaning \rts{TVar}s that are only read but not updated within the atomic block do not get locked at all, not even briefly.
This is merely a performance optimization.
It would be possible to do it in the presence of finalizers, but it complicates the implementation, since the \rts{frozen{\_}by} field is protected by the \rts{TVar} lock.
Note that this read-phase is also disabled in the original implementation whenever a transaction touches an invariant, for the very same reason.

% blocking on frozen tvars
What exactly happens when a transaction tries to commit an update but hits upon a frozen \rts{TVar}?
The transaction could simply restart, like it does whenever the \rts{current{\_}value} of a \rts{TVar} changes behind its back.
However, if the \rts{TVar} is frozen, that means it is involved in a potentially long-running finalizer, so it could still be frozen when it is time to commit again.
Since the transaction can never make progress as long as the \rts{TVar} is frozen, the only logical solution is to wait until the state of the \rts{TVar} has actually changed.%
\footnote{%
Additionally, there is an issue with GHC's optimizer being too eager when removing yield points.
The transaction would just restart over and over again, denying the thread running the finalizer the chance to actually complete.
This is a long-standing problem in GHC. See \url{http://ghc.haskell.org/trac/ghc/ticket/367}.
}
This is similar to what happens when calling the blocking operator |retry|.
In fact, we can reuse a lot of |retry|'s runtime machinery: if \rts{stmPrepareToCommitTransaction} discovers that some of the transaction's \rts{TVar}s are frozen, it calls \rts{stmWait} to put the \rts{TRec} on the \rts{TVar}s' watch queues before putting the thread to sleep.
Being on the \rts{TVar}s' watch queues means that whenever one of those \rts{TVar}s is updated in a commit, the waiting \rts{TRec} is woken up.
When this happens, \rts{stmReWait} is called to validate the \rts{TRec} again to see if it should continue waiting or if it makes sense now to restart the transaction or even to commit it right away.

%------------------------------------------------------------

\subsection{Nesting}

The original STM code was written under the assumption that nested transactions are impossible.
To ensure safety, any attempts to bypass the type system are caught at run-time.
For example, making calls of the following form will result in an error:
\begin{code}
atomically  (unsafeIOToSTM (atomically ...))
\end{code}
However, the system does implicitly support two other kinds of nesting:
(1) the |orElse| operator begins a new \rts{TRec} for each of its branches; if a branch completes successfully (i.e.\ does not retry) the \rts{TRec} is merged upwards into the enclosing record;
(2) invariants introduced by |alwaysSucceeds| are checked at commit time, where a new nested transaction is created for each invariant and completely discarded once the check is over.

The form of nesting made possible by |atomicallyWithIO|, viz.\ beginning a new transaction within the finalizer of another transaction, is similar in execution to these two, except that we want to neither merge nor discard the nested \rts{TRec}, but simply commit it as-is and then return to the old transaction.
The only difference between a regular atomic block and an atomic block begun inside a finalizer is that in the latter case the thread has a dormant \rts{TRec} it returns to once the transaction inside the finalizer has finished.
Supporting this is pretty easy, as the existing infrastructure for nested \rts{TRec}s, mainly the \rts{enclosing{\_}trec} field, can be reused.
Supporting nested transactions basically amounts to loosening some assertions about what constitutes an outermost transaction in a chain of \rts{TRec}s: previously, the outermost transaction was indicated by an empty \rts{enclosing{\_}trec} field, while now it can also be a transaction whose \rts{enclosing{\_}trec} is running a finalizer.
Everything else works exactly as it did before.

What happens when the nested transactions share variables?
The inner transaction just reading from a \rts{TVar} is never a problem.
It simply sees the \rts{current{\_}value} of the \rts{TVar} in memory.
Note that since the outer transaction has not yet committed, any updates it might have made to this \rts{TVar} are still only in its \rts{TRec}.
They are not yet globally visible, including within the transaction's own finalizer.
This matches the semantics.
As does the fact that an inner transaction cannot write to a shared \rts{TVar}.
If it tries to, it will block during commit, since the \rts{TVar} has been frozen by the outer transaction.
Meanwhile, the outer transaction is waiting for the inner transaction to finish.
The runtime system immediately detects this deadlock and throws an exception, gracefully aborting the whole transaction.

%============================================================

