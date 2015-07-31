%include thesis.fmt

\chapter{STM as a database language}
\label{chap:database}

As a qualitative demonstration of the effectiveness of both finalizers and transactional tries, I will now use them to build a real application together with a reusable framework for constructing lightweight databases.

This is partly a continuation of my previous work on the \package{tx} library \parencite{schroeder-2013}, which was the original motivation for finalizers.
The goal of that library was to add a thin persistence layer on top of STM.
But without finalizers, this could not be done in a reliable manner, for the reasons laid out in \Cref{sec:stm-and-acid}.
Now, however, we can do it right.

\section{Example: a social network}

As a running example, we will build a simple social networking site.
The full application includes a Haskell web server that exposes a RESTful API and a simple JavaScript client.
Here we will focus on the site's back-end, i.e.\ its data types and business logic.
All code snippets in this chapter are taken from fully working programs.
The complete sample code is available at \url{http://github.com/mcschroeder/social-example}.

\bigskip
Our social network will start out with a modest set of features:
users can post messages to their \emph{timelines};
users can \emph{follow} other users;
and each user has a personalized \emph{feed}, which interweaves her timeline with the timelines of all the people she follows.

Because we want to quickly arrive at a working prototype, we use STM for concurrency and postpone the question of durability until later.
This way we can focus on getting the types right for our business logic, without having to worry about interfacing with an external database just now.
This first version of the social network can be found in the \texttt{social0} folder of the sample code.

\bigskip
Speaking of types, here they are:
\begin{code}
data SocialDB = SocialDB
    {  users  :: Map UserName User
    ,  posts  :: Map PostId Post
    }
\end{code}
\begin{code}
type UserName = Text
\end{code}
\begin{code}
data User = User
    {  name       :: UserName
    ,  timeline   :: TVar [Post]
    ,  following  :: TVar (Set User)
    ,  followers  :: TVar (Set User)
    }
\end{code}
\begin{code}
newtype PostId = PostId Word64
    deriving (Eq, Ord, Random, Show, Hashable)
\end{code}
\begin{code}
data Post = Post
    {  postId  :: PostId
    ,  author  :: User
    ,  time    :: UTCTime
    ,  body    :: Text
    }
\end{code}

The whole of the network is contained in the |SocialDB| type.
Users are identified by their names and are represented by the |User| type.
Each user keeps a list of the posts on her own timeline and also keeps track of other users that she follows and that are following her.
Posts are identified by a globally unique |PostId| and are represented by the |Post| type, which contains the body of the post as well as its author and the time it was created.

Behind the |Map| type of the |users| and |posts| collections lies the transactional trie from \Cref{chap:ttrie}.
This guarantees that there will be no contention when our millions of future users all post something to our site at the same time.

Since the types contain transactional variables, computations on them must be done in the STM monad.
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
\noindent
And this is how users can follow each other:
%{
%format user = "\Varid{user}"
%format user1
%format user2
\begin{code}
follow :: User -> User -> STM ()
follow user1 user2 = do
    modifyTVar (following user1) (Set.insert user2)
    modifyTVar (followers user2) (Set.insert user1)
\end{code}
%}
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
    posts <- takeWhile isNew <$> feed user
    if null posts then retry else return posts
  where
    isNew post = diffUTCTime (time post) lastSeen > 0.1
\end{code}
%}
The |retry| operator enables |waitForFeed| to block until there are new posts in a user's feed.
Our server uses this function to effortlessly push updates to the client in real-time via HTTP long polling \parencite{loreto-et-al-2011}.

\bigskip
Creating new posts is also relatively straightforward:
%{
%format author = "\Varid{author}"
%format body = "\Varid{body}"
%format db = "\Varid{db}"
%format postId = "\Varid{postId}"
%format time = "\Varid{time}"
%format post = "\Varid{post}"
%format posts = "\Varid{posts}"
\begin{code}
createPost :: User -> Text -> SocialDB -> STM Post
createPost author body db = do
    postId <- newUniquePostId db
    time <- unsafeIOToSTM getCurrentTime
    newPost postId author time body db
\end{code}
\begin{code}
newUniquePostId :: SocialDB -> STM PostId
newUniquePostId db = do
    postId <- unsafeIOToSTM randomIO
    alreadyExists <- Map.member postId (posts db)
    check (not alreadyExists)
    return postId
\end{code}
\begin{code}
newPost :: PostId -> User -> Time -> Text -> SocialDB -> STM Post
newPost postId author time body db = do
    let post = Post {..}
    modifyTVar (timeline author) (post:)
    Map.insert postId post (posts db)
    return post
\end{code}
%}
In order to generate a random ID for the post and to get the current time, we have to use |unsafeIOToSTM| to interleave I/O actions with the transaction.
In these cases, this is perfectly safe:
we are either only reading from the outside world (|getCurrentTime|) or are otherwise producing only harmless side effects (|randomIO|, which updates the internal state of its random number generator).
If the transaction aborts, no harm is done.
If the transaction retries, we simply get a new time and generate a new random ID, which is just what we want.

We have now seen pretty much the whole back-end of the social network, apart from |createUser|, which is not that different from |createPost|.
Using STM, we managed to arrive very quickly at a functional prototype.
Of course, since we are using \emph{only} STM, all effects stay purely in memory.
If the server is shut down, the data is gone.
In the next section, we will use finalizers to change that.

\section{The TX monad}

The idea is to use an STM finalizer to record state-changing operations in a write-ahead log file.
We do not serialize the data itself, but rather the operations on the data.
We record function calls that can later be replayed to restore the state of the system at the time of the recording.
A similar scheme has been implemented by the \package{acid-state} library \parencite{himmelstrup-2014}, which uses a custom lock-based state container instead of building on STM.
The fact that \package{acid-state} is used by Hackage, the official Haskell package repository, demonstrates the practicality of such an approach.

To facilitate the recording of operations, a thin layer on top of |STM| is introduced: the |TX| monad.
Here is the |follow| function from the previous section, rewritten in |TX|:
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
This demonstrates the two main tasks we do in |TX|: lifting functions from the underlying |STM| monad using |liftSTM| and recording database operations using |record|.
Note how |TX| is parameterized by the type of database, in our case |SocialDB|.

An updated version of the social network can be found in the \texttt{social1} folder of the sample code.
Below is |createPost| rewritten for |TX|.
The |getData| function is used to retrieve the database implicitly carried around by |TX|:
%{
%format author = "\Varid{author}"
%format body = "\Varid{body}"
%format db = "\Varid{db}"
%format postId = "\Varid{postId}"
%format time = "\Varid{time}"
\begin{code}
createPost :: User -> Text -> TX SocialDB Post
createPost author body = do
    db <- getData
    postId <- liftSTM $ newUniquePostId db
    time <- unsafeIOToTX getCurrentTime
    record $ NewPost postId (name author) time body
    liftSTM $ newPost postId author time body db
\end{code}
%}
Notice that the |TX| version of |createPost| is virtually identical to the pure STM version, except for a few lifts and the call to |record|.
The existing |STM| functions |newPost| and |newUniquePostId| could be reused and did not have to be touched at all.

\bigskip
The implementation of |TX| itself is pretty simple.
It is a strict state transformer\footnote{The \package{transformers} package \parencite{gill-paterson-2014} provides a variety of monad transformers.} on top of |STM|, keeping a list of recorded operations, as well as allowing easy access to the root database type:
%{
%format op = "\Varid{op}"
%format log = "\Varid{log}"
\begin{code}
newtype TX d a = TX (StateT (d, [Operation d]) STM a)
  deriving (Functor, Applicative, Monad)

record :: Operation d -> TX d ()
record op = TX $ modify $ \(d,log) -> (d,op:log)

getData :: TX d d
getData = TX $ gets fst

liftSTM :: STM a -> TX d a
liftSTM = TX . lift
\end{code}
%}
|TX| could be implemented in a more strongly typed fashion, using a combination reader/writer monad to emphasize that the database reference |d| is constant and that the operation log is strictly write-only.
Alas, using |StateT| is much more efficient, in addition to being more convenient, as writer transformers do not work in constant space.\footnote{See \url{https://mail.haskell.org/pipermail/libraries/2013-March/019528.html}.}

The argument to |record| is an |Operation d|, which is an associated type of the |Database| class:
\begin{code}
class Database d where
    data Operation d
    replay :: Operation d -> TX d ()
\end{code}
Associated types \parencite{chakravarty-et-al-2005} are defined by concrete instances of the class they are associated with.
In our case, the |Database| instance of |SocialDB| defines |Operation SocialDB|, denoting operations that can be replayed in the context of |SocialDB|.
The constructors of |Operation SocialDB| represent the functions that recorded them.
For example, the |Follow| and |NewPost| constructors represent the |follow| and |newPost| functions:
%{
%format db = "\Varid{db}"
%format user = "\Varid{user}"
%format user1
%format user2
%format name = "\Varid{name}"
%format name1
%format name2
%format postId = "\Varid{postId}"
%format time = "\Varid{time}"
%format body = "\Varid{body}"
%format author = "\Varid{author}"
\savecolumns
\begin{code}
instance Database SocialDB where
    data Operation SocialDB  =  Follow UserName UserName
                             |  NewPost PostId UserName UTCTime Text
                             |  ...
\end{code}
\clearpage
\restorecolumns
\begin{code}
    replay (Follow name1 name2) = do
        user1 <- getUser name1
        user2 <- getUser name2
        user1 `follow` user2

    replay (NewPost postId name time body) = do
        db <- getData
        author <- getUser name
        liftSTM $ void $ newPost postId author time body db
\end{code}
%}
So the |follow| function |record|s the |Follow| type and |replay|ing the |Follow| type causes the |follow| function to be called again.
Note that |TVar|s and types that contain |TVar|s are not directly serializable.
Therefore the |Follow| type contains only the names of the users.
During |replay|, the full |User| structure is fetched from the database.

\bigskip
Now, how is a |TX| computation actually performed and its effects serialized?
The |TX| equivalent to |atomically| is |durably|, and this is where finalizers finally come into play:
\begin{code}
durably :: DatabaseHandle d -> TX d a -> IO a
durably h (TX m) = atomicallyWithIO action finalizer
  where
    action                  = runStateT m (database h, [])
    finalizer (a, (_,ops))  = serialize ops h >> return a
\end{code}
The |durably| function runs the state monad inside |TX| and performs the resulting |STM| |action| using |atomicallyWithIO|, passing the logged operations to the |finalizer|, which serializes them and returns the transaction's final result.
|DatabaseHandle d| is an opaque type referring to a specific database and its on-disk representation.
The application programmer uses the function
\begin{code}
openDatabase  ::  (Database d, SafeCopy (Operation d))
              =>  FilePath -> d -> IO (DatabaseHandle d)
\end{code}
to acquire a database handle that can be shared freely among threads.
Upon opening a database, its in-memory representation |d| is initialized by replaying the stored operations from the underlying log file.

Note the |SafeCopy| constraint on |Operation d|.
It comes from the widely-used \package{safecopy} library \parencite{himmelstrup-lessa-2014}, which is a serialization library with support for version control.
By leveraging \package{safecopy}, |TX| supports schema migration right out of the box.
The ability to extend and change data types and business logic while keeping compatibility with old database files is very important for any database system, and maybe especially so for something as lightweight as |TX|, which naturally lends itself to rapid prototyping.
For a practical demonstration of this, look in the \texttt{social2} directory of the sample code.
It contains a version of the social network that has some additional features, such as allowing users to post messages directly to the timelines of other users and adding the ability to ``like'' posts.
Implementing these features necessitated non-trivial changes to the core types, yet switching from an older version of the serialized database is seamlessly possible.

\section{Caveats}

Due to the simplicity of the design and the nature of the underlying STM abstraction, there are some drawbacks and limitations.
They are not fundamental, but they indicate that in order to achieve higher scalability (in terms of application complexity), a more sophisticated interface might be needed.

\paragraph{Using exceptions to handle failure.}
In some of the code above we have used a function |getUser| to retrieve a user from the database by name.
But what happens if there is no user for the given name?
Usually, we want to make the possibility of failure explicit in the types and so would expect |getUser| to have a type like
\begin{code}
getUser :: UserName -> TX SocialDB (Maybe User)
\end{code}
However, consider the following scenario:
%{
%format db = "\Varid{db}"
%format eve = "\Varid{eve}"
%format adam = "\Varid{adam}"
\begin{code}
do  eve <- newUser "Eve"
    adam <- getUser "Adam"
    case adam of
        Just adam  -> eve `follow` adam
        Nothing    -> return ()
\end{code}
If |getUser| returns |Nothing|, then even though the block returns without further action, |eve| is still created when the transaction ultimately commits.
Of course this particular example is somewhat contrived, as we could easily rearrange the functions to postpone the creation of |eve| until after we have established that |adam| exists.
But you can imagine that this may not always be possible and does not scale well.
%}

The real problem is that there is no way to explicitly abort an STM transaction other than throwing an exception.
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
    liftSTM $ do
        user <- Map.lookup name (users db)
        case user of
            Just user  -> return user
            Nothing    -> throwSTM (UserNotFound name)
\end{code}
%}

In the majority of uses, the desired outcome of a function like |getUser| in case the user is not found is to abort the transaction.
There are certainly ways to avoid spooky exceptions as much as possible, e.g.\ by adding an exception monad transformer layer to |TX| (something like |Control.Monad.Trans.Except| from \package{transformers}).
But in the end a real exception would need to be thrown regardless, in order to signal to the STM runtime that the transaction needs to be aborted.
I have favored the simpler design and it seems to work well in practice, as indicated by our example application.

\paragraph{Double replay.}
The |record| function gives us great flexibility in how and when to record operations.
But with great flexibility comes great responsibility:
not only does the programmer have to remember to actually record a specific operation, she must be especially careful when composing functions that record something.
Consider the following situation:
\begin{code}
f  = record F >> g
g  = record G
\end{code}
Running |f| first records |F| and then calls |g| and so also records |G|.
Replaying this recording will first replay |F|, which runs |f| which calls |g|, and then it will replay |G|, which calls |g| again.
We have now called |g| twice, even though it was only executed once during the original run!
This is unfortunate, because it means that to avoid such situations, the user has to keep the call graph of recordable functions in mind, and there is no help from the compiler.

This makes one wonder: why do we record on this semi-high level\,---\,that obviously is not high enough to save us from such basic yet easily overlooked mistakes\,---\,instead of just directly recording every primitive |writeTVar| operation?
We may not be able to serialize a |TVar| itself, but we can certainly serialize its contents.
Could we not have a function
\begin{code}
durableWriteTVar :: Serializable a => TVar a -> a -> TX d ()
\end{code}
that automatically logs updates at the lowest level, removing the need for explicit calls to |record|?
Alas, this is not possible: during replay, there is no way to automatically re-associate the recorded value with the |TVar| it originally belonged to.
We always need some kind of context to find the one place in our data structure that has the correct |TVar| to put the value back into.
By recording higher-level operations, this context is automatically included.
