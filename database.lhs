%include thesis.fmt

\chapter{STM as a database language}
\label{chap:database}

\red{TODO: this chapter is still in very rough shape}
\bigskip

I now present a reusable framework for constructing application-specific databases, built on STM with finalizers.
This is a continuation of my previous work on the \package{tx} library \parencite{schroeder-2013}, which was the original motivation for finalizers.

What is an ``application-specific'' database?
Fundamentally, such a database is little more than a collection of regular Haskell data types, using pure Haskell as its query language.
There is no external data format and no type conversions are necessary.
By lifting the familiar functions from the underlying STM abstraction, data can be manipulated directly.
There is little separation between functions over the database and the application's business logic.
The database layer is very thin.

Durability is enabled by a write-ahead log recording all operations on the database, which otherwise exists solely in-memory.
A similar scheme has been implemented by the \package{acid-state} library \parencite{himmelstrup-2014}, which uses a custom lock-based state container instead of deferring to STM.
The fact that \package{acid-state} is used by Hackage\footnote{\url{http://hackage.haskell.org}}, the official Haskell package repository, demonstrates the practicality of such a database design.

All code snippets from this and subsequent chapters are taken directly from fully working prototypes.
The complete sample code is available at \url{https://github.com/mcschroeder/stmfin-examples}. %TODO hackage?

\section{Example: a social network}

As a motivating example, we will build a simple social networking site.
The full application, which includes a Haskell web server that exposes a RESTful API and a simple JavaScript client, can be found in the directory \texttt{social1} in the sample code.
Here, we will focus mainly on the site's back-end, i.e.\ its database and business logic.

Our social network will start out with a modest set of features:
\begin{itemize}
\item Users can post messages to their \emph{timelines}.
\item Users can \emph{follow} other users.
\item Each user has a personalized \emph{feed}, which interweaves her timeline with the timelines of all the people she follows.
\end{itemize}

Let's look at some types:
\begin{code}
data SocialDB = SocialDB
    {  users  :: TVar (Map UserName User)
    ,  posts  :: TVar (Map PostId Post)
    }

data User = User
    {  name       :: UserName
    ,  timeline   :: TVar [Post]
    ,  following  :: TVar (Set User)
    ,  followers  :: TVar (Set User)
    }

data Post = Post
    {  postId  :: PostId
    ,  author  :: User
    ,  time    :: UTCTime
    ,  body    :: Text
    }

type UserName = Text
newtype PostId = PostId Word64
\end{code}

Users are identified by their name and are represented by the |User| type.
Each user keeps her own list of the posts on her timeline and also keeps track of other users that she follows and that are following her. 
Posts are identified by a globally unique |PostId| and are represented by the |Post| type, which contains the body of the post as well as its author and the time it was created.
The whole of the social network is contained in the |SocialDB| type.

Since our types contain transactional variables, computations must be done in the STM monad. 
For example, this is how we compute a user's feed:
%{
%format user = "\Varid{user}"
%format myPosts = "\Varid{myPosts}"
%format others = "\Varid{others}"
%format otherPosts = "\Varid{otherPosts}"
\begin{code}
feed :: User -> STM [Post]
feed user = do
    myPosts <- readTVar (timeline user)
    others <- Set.toList <$> readTVar (following user)
    otherPosts <- concat <$> mapM (readTVar . timeline) others
    return $ sortBy (flip $ comparing time) (myPosts ++ otherPosts)
\end{code}
%}
%TODO more efficient merge sort

By using STM, we can write high-level code and get atomicity for free.
Additionally, we can take advantage of some other nice STM features, like composable blocking:
%{
%format user = "\Varid{user}"
%format lastSeen = "\Varid{lastSeen}"
%format post = "\Varid{post}"
%format posts = "\Varid{posts}"
\begin{code}
waitForFeed :: User -> UTCTime -> STM [Post]
waitForFeed user lastSeen = do
    let isNew post = diffUTCTime (time post) lastSeen > 0.1
    posts <- takeWhile isNew <$> feed user
    if null posts then retry else return posts
\end{code}
%}
The |retry| operator enables |waitForFeed| to block until there are new posts in a user's feed.
Our server uses this function to effortlessly implement HTTP long polling \parencite{loreto-et-al-2011}.

%=============================================================================

\section{The TX monad}

If we want to do more than just query the database, we need to go beyond the standard |STM| monad.
To perform durable updates, we use a monad called |TX|, which is built on top of |STM| and lets us record database operations.
The idea is not to serialize the data itself, but rather to log the function calls that are responsible for updating the data, so that later on they can be replayed to restore the last consistent state of the database.

The following example demonstrates the two main things we do in |TX|: lifting functions from the underlying |STM| monad using |liftSTM| and recording database operations using |record|.
Note how |TX| is parameterized by the type of database, in our case |SocialDB|:
%{
%format user = "\Varid{user}"
%format user1
%format user2
\begin{code}
follow :: User -> User -> TX SocialDB ()
follow user1 user2 = do
    record $ Follow (name user1) (name user2)
    liftSTM $ do
        modifyTVar (following user1) (Set.insert user2)
        modifyTVar (followers user2) (Set.insert user1)
\end{code}
%}
Because it is not possible to simply serialize functions, |record| takes a type denoting a particular operation:
\begin{code}
record :: Operation d -> TX d ()
\end{code}
where |Operation d| is an associated type \parencite{chakravarty-et-al-2005} of the |Database| class:
\begin{code}
class Database d where
    data Operation d
    replay :: Operation d -> TX d ()
\end{code}

In the above example, the |follow| function is denoted by the |Follow| constructor of the |Operation SocialDB| type, which is declared as part of the |Database| instance for |SocialDB|:
%{
%format user = "\Varid{user}"
%format user1
%format user2
%format name = "\Varid{name}"
%format name1
%format name2
\begin{code}
instance Database SocialDB where
    data Operation SocialDB = ... | Follow UserName UserName |  ...

    replay (Follow name1 name2) = do
        user1 <- getUser name1
        user2 <- getUser name2
        user1 `follow` user2
\end{code}
%}

A database is simply any type that declares operations that can be replayed in the |TX| monad.
Any additional constraints on the |Operation d| type, e.g.\ how it is to be transformed into bits for serialization, are up to the storage back-end, which will be discussed in a short while.
For now, you may have noticed how the |Follow| constructor takes as arguments the names of the users, instead of the users themselves.
This is because the |User| data type is not serializable, regardless of any possible constraints put forth by the storage back-end, since it contains |TVar|s.
Thus we have to fetch the users again during |replay|.

But instead of recording these high-level operations, why not simply record every |writeTVar|?
We may not be able to serialize a |TVar| but we could serialize the \emph{contents} of a |TVar|.
Could we not have a function
\begin{code}
durableWriteTVar :: Serializable a => TVar a -> a -> TX d ()
\end{code}
that automatically logs updates at the lowest level, removing the need for explicit calls to |record|?
Alas, this is not possible: during replay, there is no way to automatically re-associate the recorded value with the |TVar| it originally belonged to.
We always need some kind of context to find the one place in our data structure that has the correct |TVar| to put the value back into.
By recording only high-level operations, we get this context for free.

We also get greater flexibility in how and when to record operations. With greater flexibility comes greater responsibility, however:
not only does the programmer have to remember to actually record a specific operation, she must be especially careful when composing functions that record something.
Consider the following situation:
\begin{code}
f  = record F >> g
g  = record G
\end{code}
Running |f| first records |F| and then calls |g| and so also records |G|.
Replaying this recording will first replay |F|, which runs |f| which calls |g|, and then it will replay |G|, which calls |g| again.
We've now called |g| twice, even though it was only executed once during the original run!

That such things are possible is unfortunate, but it keeps the design simple.
In appendix TODO I present a different design, based on effectful monads, that aims to eliminate these trade-offs using advanced type system features. %TODO reference correct appendix

\bigskip

% error handling
Before we go on to the implementation of |TX|, let us briefly look at how it handles failure.
For example, what happens if |getUser| can not find a name in the database?
Usually, we would want to make possible failure explicit via types, and so we would expect |getUser| to have a type like
\begin{code}
getUser :: Text -> TX SocialDB (Maybe User)
\end{code}
However, consider the following scenario:
%{
%format eve = "\Varid{eve}"
%format adam = "\Varid{adam}"
\begin{code}
do  eve <- newUser "Eve"
    adam <- getUser "Adam"
    case adam of
        Just adam  -> eve `follow` adam
        Nothing    -> return ()
\end{code}
If |getUser| returns |Nothing|, then even though the block returns without further action, |eve| is still created, since |newUser| was called at the very beginning.
Of course this particular example is somewhat contrived, as we could easily rearrange the functions to postpone the creation of |eve|.
But you can imagine that this may not always be possible and does not scale well.
%}

The real problem is that there is no way to explicitly abort an STM transaction.
This is why |getUser| is actually implemented like this:
%{
%format name = "\Varid{name}"
%format db = "\Varid{db}"
%format usermap = "\Varid{usermap}"
%format user = "\Varid{user}"
\begin{code}
getUser :: UserName -> TX SocialDB User
getUser name = do
    db <- getData
    usermap <- liftSTM $ readTVar (users db)
    case Map.lookup name usermap of
        Just user  -> return user
        Nothing    -> throwTX (UserNotFound name)
\end{code}
%TODO update sample code to also use actual exceptions
%}
where |throwTX| is simply
\begin{code}
throwTX :: Exception e => e -> TX d a
throwTX = liftSTM . throwSTM
\end{code}

We are using exceptions, instead of encoding failure into the types, because exceptions are the only way to properly abort an STM transaction.
In the majority of uses of a function like |getUser|, the desired outcome in case the user is not found is to abort the transaction.
There are certainly ways to avoid those spooky exceptions as much as possible, such as making |TX| an exception/error/failure monad.
But in the end an exception would have to be thrown somewhere regardless.
I have again favored the simpler design and it seems to work well in practice.

%=============================================================================

\section{Implementation}
Internally, the |TX| monad is a combination reader/writer monad.
It has a pretty straightforward implementation:\footnote{To be found in the directory \texttt{tx1} in the sample code.}
%{
%format op = "\Varid{op}"
%format ops = "\Varid{ops}"
%format ops'
\begin{code}
data TX d a = TX { unTX :: d -> STM (a, [Operation d]) }

instance Functor (TX d) where
  fmap f m = TX $ \d -> do   (a, ops) <- unTX m d
                             return (f a, ops)

instance Applicative (TX d) where
  pure   = return
  (<*>)  = ap

instance Monad (TX d) where
  return a  = TX $ \ _ -> return (a, [])
  m >>= k   = TX $ \d -> do   (a, ops ) <- unTX m d
                              (b, ops') <- unTX (k a) d
                              return (b, ops ++ ops')

record :: Operation d -> TX d ()
record op = TX $ \ _ -> return ((), [op])

liftSTM :: STM a -> TX d a
liftSTM m = TX $ \ _ -> m >>= \a -> return (a, [])

getData :: TX d d
getData = TX $ \d -> return (d, [])
\end{code}
%TODO maybe need throwTX in here?

The reader part of the monad, viz.\ keeping around the database |d|, is not necessary for the main purpose of recording database operations.
It is however very convenient to have a function like |getData|, allowing the programmer easy access to the root database type from within any |TX| action.

Now, how is a |TX| computation actually performed?
The |TX| equivalent for |atomically| is |runTX|, and this is where finalizers finally come into play:
\begin{code}
runTX :: DatabaseHandle s d -> TX d a -> IO a
runTX h m = atomicallyWithIO action finalizer
  where
    action              = unTX m (database h)
    finalizer (a, ops)  = serializer h ops >> return a
\end{code}
The first argument to |runTX| is a |DatabaseHandle|, which is an abstraction over the concrete storage and serialization mechanism chosen by the programmer:
\begin{code}
data DatabaseHandle s d = DatabaseHandle
    {  database    :: d
    ,  storage     :: s
    ,  serializer  :: [Operation d] -> IO ()
    }
\end{code}

For the social network example, I implemented binary serialization to an append-only log file, using the \package{cereal} \parencite{kolmodin-et-al-2013} and \package{safecopy} \parencite{himmelstrup-lessa-2014} libraries.
%TODO footnote location of LogFile module
The user of this particular storage backend can obtain a |DatabaseHandle| by calling a function named |openDatabase|, which deserializes and replays a previously recorded database log, if available, and then prepares the log file for further serialization.
Its implementation is given below.\footnote{The full module can be found in the sample code under \texttt{tx1/TX/LogFile.hs}}
Note how the |SafeCopy| class constraint on |Operation| is local to this storage module; |Database| and the |TX| operations can stay completely agnostic as to how serialization is actually done.
%{
%format fp = "\Varid{fp}"
%format fileHandle = "\Varid{fileHandle}"
\begin{code}
data LogFile = LogFile { fileHandle :: MVar Handle }

openDatabase  :: (Database d, SafeCopy (Operation d))
              => FilePath -> d -> IO (DatabaseHandle LogFile d)
openDatabase fp d = do
    fileHandle <- mkFileHandle fp
    deserialize fileHandle d
    return DatabaseHandle  {  database    = d
                           ,  storage     = LogFile fileHandle
                           ,  serializer  = serialize fileHandle
                           }
\end{code}
%}

% TODO checkpoints
% its not in the sample code, but another backend could also implement checkpoints

Since the |LogFile| back-end is built on |SafeCopy|, it supports schema migration right out of the box.
Look in the \texttt{social2} directory of the sample code for a version of our social network that has some additional features, such as users being able to like posts and to post messages to the timelines of other users.
It is a practical example of how to extend and change data types and business logic while keeping compatibility with old database files.
