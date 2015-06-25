%include thesis.fmt

\chapter{Motivation}
\label{chap:motivation}

It is a widely held opinion that concurrent programming is difficult and error-prone.
Low-level synchronization mechanisms, such as locks, are notoriously tricky to get right.
Deadlocks, livelocks, heisenbugs and other issues encountered when writing complex concurrent systems are usually hard to track down and often confound even experienced programmers.

To simplify concurrent programming, higher-level abstractions are needed.
One such abstraction is \emph{Software Transactional Memory} (STM).
Briefly, this technique allows the programmer to group multiple memory operations into a single atomic block, not unlike a database transaction.
When implemented in a high-level language such as Haskell, with its emphasis on purity and its strong static type system, STM becomes especially powerful. 

However, it is not always as powerful as we would like it to be:
\begin{itemize}
\item While manipulating memory using STM is easy, persisting those manipulations in a transactionally safe way is frustratingly impossible.
There is no way to achieve \emph{durability}, an important component of database transactions.
\item Some pure data types, such as maps, can be highly inefficient when combined with STM.
The reason is \emph{contention}: certain common access patterns cause unreasonably high numbers of transactional conflicts.
\end{itemize}

In this thesis, I will address these problems by combining transactions with side-effects.
I present two approaches of doing this safely: one from the top down, and one from the bottom up.
In particular, the contributions of my work are:
\begin{itemize}
\item A new STM primitive called |atomicallyWithIO| that allows the user to attach a \emph{finalizer} to an STM transaction.
Such finalizers are arbitrary I/O actions that can be used to serialize transactional data or incorporate external information into a transaction.
I provide a detailed discussion of the semantics of this extension to STM, as well as my implementation of it in the Glasgow Haskell Compiler. (\Cref{chap:finalizers})

\item A demonstration of the effectiveness of finalizers by constructing a thin database abstraction on top of STM.
I then use this to build a simple social networking site. (\Cref{chap:database})

\item A contention-free STM data structure, the \emph{transactional trie}.
It is based on the concurrent trie, but lifted into an STM context and combines transactions with carefully considered internal side effects.
I present its design and implementation in Haskell, and evaluate it against other STM-specialized data structures. (\Cref{chap:ttrie})
\end{itemize}

\noindent
In the remainder of this introductory chapter, I will give a brief overview of Haskell's STM interface.

%==========================================

\section{STM in Haskell}

Here are the main data types and operations of STM in Haskell:
\begin{code}
data STM a
instance Monad STM

atomically :: STM a -> IO a

data TVar a
newTVar :: a -> STM (TVar a)
readTVar :: TVar a -> STM a
writeTVar :: TVar a -> a -> STM ()

retry :: STM a
orElse :: STM a -> STM a -> STM a
\end{code}

% STM , TVar, atomically
Atomic blocks in Haskell are represented by the |STM| monad.
Inside this monad, we can freely operate on transactional variables, or |TVar|s.
We can read them, write them and create new ones.
When we want to actually perform an STM computation and make its effects visible to the rest of the world, we apply |atomically| to the computation.
This function turns an STM block into a transaction in the |IO| monad that, when executed, will take place atomically with respect to all other transactions.

For example, the following code snippet increments a transactional variable |v|:
\begin{code}
atomically $ do  x <- readTVar v
                 writeTVar v (x+1)
\end{code}
The use of |atomically| guarantees that no other thread can come in between the reading and writing of the variable.
The sequence of operations happens indivisibly.

% composability
An important aspect of Haskell's STM implementation is that it is fully composable.
Smaller transactions can be combined into larger transactions without having to know how these smaller transactions are implemented.
An important tool to make this possible is the composable blocking operator |retry|.
%
% retry
Conceptually, |retry| abandons the current transaction and runs it again from the top.
In the following example, the variable |v| is decremented, unless it is zero, in which case the transaction blocks until |v| is non-zero again.
\begin{code}
atomically $  do x <- readTVar v
              if x == 0
                 then retry
                 else writeTVar v (x-1)
\end{code}
%
% orElse
In addition to |retry|, there is the |orElse| combinator, which allows ``trying out'' transactions in sequence.
|m1 `orElse` m2| first executes |m1|; if |m1| returns, then |orElse| returns; but if |m1| retries, its effects are discarded and |m2| is executed instead.

% exceptions
STM is also robust against exceptions.
The standard functions |throw| and |catch| act as expected:
if an exception occurs inside an atomic block and is not caught, the transaction's effects are discarded and the exception is propagated.

% invariants
Another interesting part of Haskell's STM are data invariants.
Using the |alwaysSucceeds| function, one can introduce an invariant over transactional variables that is dynamically checked on every atomic update.
%This mechanism is implemented in a surprisingly efficient manner.

For more background on Haskell's STM, including its implementation, see the original STM papers \parencite{harris-et-al-2005, harris-peytonjones-2006}.
For a more thorough exploration of not only STM but also other Haskell concurrency mechanisms, read Simon Marlow's excellent book on that topic~\parencite{marlow-2013}.
