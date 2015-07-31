%include thesis.fmt

\chapter{Conclusions \& Perspectives}
\label{chap:conclusions}

I have presented two new approaches of combining transactions with side-effects, with the goal of adding \emph{durability} and eliminating \emph{contention}:

\begin{itemize}
\item \textbf{STM Finalizers} are a global, top-down approach of adding arbitrary I/O during transactional commit.
I have given a precise semantics of this extension to Haskell's STM system, as well as an implementation in the Glasgow Haskell Compiler.
I have given many examples of the usefulness of finalizers, and demonstrated how they make it practical and easy to build a lightweight database application on top of STM.

\item The \textbf{transactional trie} is a specialized data structure that uses local side-effects to eliminate contention inside STM.
I have discussed its implementation in detail and reasoned about its safety and related trade-offs.
I have shown the superior performance of the transactional trie relative to similar data structures.
\end{itemize}

\section{Related Work}

Twilight STM \parencite{bieniusa-2011a} is ``a transactional memory system [that] augments transactions with non-reversible operations and allows introspection and modification of a transaction's state.''
It has a library-level implementation in Haskell, available in the \package{twilight-stm} package \parencite{bieniusa-2011b}, which provides a custom STM monad.
Twilight STM distinguishes between three transactional phases:
the \emph{atomic} phase is the same as in standard STM;
in the \emph{twilight} phase, inconsistencies between the transactional state and the global state can be detected and fixed, or otherwise ignored;
and once in the \emph{safe} phase, a transaction is guaranteed to commit, unless explicitly retried, but the transaction's write set can still be modified and irrevocable I/O actions can be performed.

The ability to check for and ignore transactional conflicts during the twilight phase enables the implementation of contention free data structures similar to the transactional trie.
For example, a lookup operation on a singly-linked list could safely ignore any inconsistencies arising due to concurrent inserts, thereby increasing performance.

The safe phase of Twilight STM is similar to finalizers, and somewhat more powerful:
in addition to safely performing I/O actions, it also allows limited reading and writing of transactional variables.
Unlike finalizers, however, Twilight STM does not support nesting of transactions inside its safe phase.

Since \package{twilight-stm} is a standalone library, without GHC runtime support, its performance is drastically reduced compared to native STM.
It is also not compatible with existing STM code.

\medskip

The |AdvSTM| monad from the \package{stm-io-hooks} package was already mentioned in \Cref{sec:finalizers-related-work}.
In addition to the design differences described there (it is bottom-up rather than top-down; it allows for higher composability, even though this may be problematic in conjunction with I/O), it also has the drawback of being incompatible with existing STM code.

\medskip

There have been a number of other STM implementations trying to reconcile transactions and side-effects, but all in the context of weakly typed, non-functional languages:

\textcite{welc-et-al-2008} describe a transactional memory system for Java that allows transitioning into an irrevocable state in which a transaction will no longer roll back.
Any subsequent actions, like I/O effects, will never be revoked or repeated.
They use a technique called single-owner read locks and allow only one irrevocable transaction to run at a time.

\textcite{harris-2005} extends a Java STM system with external actions, which can perform operations directly on the heap of a given context.
When an atomic block finishes, it promotes heap updates from nested contexts up to its parent context.

\textcite{spear-et-al-2008} compare several mechanisms for ``inevitable'' transactions, which cannot abort and of which only one can run at a time.

JudoSTM \parencite{olszewski-et-al-2007} is a system that uses dynamic binary-rewriting to transform C and C++ applications to support transactional execution.
It has a concept of ``privileged'' transactions, which cannot be rolled back and can be used to make system calls.

The xCall interface for the Intel STM compiler \parencite{volos-et-al-2009} enables transactions to make system calls, through a combination of delaying the execution of calls until the transaction commits and undoing the side effects of some immediately executed calls should the transaction abort.

\medskip

\textcite{sonmez-et-al-2007} extend Haskell's STM with an |unreadTVar| operation that removes a transactional variable from the read set of a transaction.
A data structure like a linked list normally has a read set that is directly proportional to the length of the list, increasing the potential for false conflicts and aborts.
By unreading |TVar|s, one can traverse a list with a small fixed-size read set.
This results in much faster execution times, although it can lead to unsafe situations, especially when composing with functions that are not aware of |unreadTVar|.

\medskip

Transactional boosting \parencite{herlihy-koskinen-2008} is a methodology for transforming highly-concurrent linearizable objects into highly-concurrent transactional objects, provided they satisfy certain regularity properties.
For an object to be boosted, its methods must be commutative, or otherwise be protected by an abstract lock, and they must have fast inverses, which will be executed if the transaction fails to commit; the data type itself is treated like a black box.
\textcite{du-bois-et-al-2014} describe a |boost| function for Haskell, as an extension to a high-level STM implementation \parencite{du-bois-2011}.

The transactional trie is not a boosted version of the concurrent trie.
The lock-free concurrent trie does not seem particularly amenable to transactional boosting, since to ensure commutativity and preserve isolation, some operations would need to acquire locks and introduce additional bookkeeping.
The benefits of reusing an existing concurrent trie implementation via transactional boosting are somewhat reduced by the necessity of explicit locking, which decreases performance and adds significantly to overall complexity, on top of the machinery necessary to enable boosting in the first place.

\section{Future Work}

Finalizers allow transactions to be interactive. 
In particular, they enable interaction with remote systems.
Building a distributed transactional memory system on top of finalizers would be the next logical step, leveraging existing distributed computation frameworks such as Cloud Haskell \parencite{epstein-2011, coutts-et-al-2015}.
It would be interesting to see if and how such a system could be implemented using just finalizers, and how it would compare to more fundamental approaches like the \package{DSTM} library \parencite{kupke-2010} or Decent STM \parencite{bieniusa-2011a}.

\medskip

By allowing nesting of transactions, finalizers can truly be arbitrary I/O actions, including those that are themselves composed of STM transactions\,---\,like parts of the Cloud Haskell platform.
But in the current design, programmer error in the form of circular dependencies between inner and outer transactions is only detected at runtime, even though it should be possible to do this statically.
One could write a compiler plugin or, more radically, rewrite the STM API to be more strongly typed, perhaps by using an effect system \parencite{orchard-petricek-2014}.
Such a new API would be incompatible with current STM code, but a full redesign of the interface could open up other possibilities.

\medskip

The TX monad could also benefit from a more strongly typed API, as a lot of bookkeeping is still up to the programmer.
Different approaches to designing a persistency layer on top of finalizers should be explored.
Additionally, there are some low-hanging fruits in the current design:
creating checkpoints of the database state at regular intervals and compressing the on-disk log files would save memory and greatly reduce replay time at startup.
There is also the possibility of making the database distributed, with or without relying on a wholly distributed STM system underneath.

\medskip

The concurrent trie, on which the transactional trie is based, supports an efficient non-blocking snapshot operation \parencite{prokopec-et-al-2012}.
This allows, for example, to fold over the trie while it is concurrently modified, without loss of consistency.
It is not clear if it is possible to lift this operation into the transactional setting as well, while keeping the same performance characteristics, but further exploration is needed.
