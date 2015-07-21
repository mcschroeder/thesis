%include thesis.fmt

\begin{abstract*}

Software Transactional Memory (STM) immensely simplifies concurrent programming by allowing memory operations to be grouped together into atomic blocks.
Like database transactions, STM transactions provide atomicity, consistency and isolation.
Unlike databases, they do not provide \emph{durability}.

I extend Haskell's STM implementation with a mechanism to add \emph{finalizers} to atomic blocks.
The new operation |atomicallyWithIO| allows the programmer to safely execute arbitrary I/O during the commit-phase of a transaction.
This can be used to make STM durable, but it turns out to have even more applications.
For example, a finalizer could ask the user to approve pending results, enabling interactive transactions.

Another common problem with STM is \emph{contention}.
When used in a transactional setting, many standard data structures cause unreasonably high numbers of conflicts.
I propose a new STM data structure, the \emph{transactional trie}, a contention-free hash map.
It is based on the lock-free concurrent trie, and uses localized side-effects to eliminate unnecessary conflicts while preserving transactional safety.

Both finalizers and the transactional trie are examples of combining transactions with side-effects.
Finalizers are a general top-down approach, while the transactional trie incorporates side effects on the micro-level.
I demonstrate the effectiveness of both by building a full sample application that uses STM as a database language, providing durability and avoiding contention.

\end{abstract*}
