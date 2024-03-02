{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds, RankNTypes, FlexibleInstances, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Opaque monadic tasks, whose inputs and outputs can be dynamic.
module Build.Task.Opaque where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.List (isInfixOf)
import Debug.Trace
import System.FilePath

-- | Environment variables are identified by names.
type Variable = String

-- | A collection of keys for accessing files, environment variables, and
-- contents of directories. Directories are somewhat magic, because their values
-- are derived from 'File' keys, i.e. creating a new file in a directory
-- requires updating the the corresponding 'Dir' key.
data Key a where
    File :: FilePath -> Key String     -- ^ File contents.
    Env  :: Variable -> Key String     -- ^ Environment variable.
    Dir  :: FilePath -> Key [FilePath] -- ^ Directory contents.

-- | Read a key's value in a computation context @f@.
type Get k f = forall a. k a -> f a

-- | Write a key's value in a computation context @f@. Note: the type can be
-- changed to @forall a. k a -> f a -> f a@ to allow for static analysis of
-- applicative and selective build tasks, since we cannot have @a@ in a static
-- context @f@, e.g. in @Const@. See more details in Section 5.3 of this paper:
-- https://www.staff.ncl.ac.uk/andrey.mokhov/selective-functors.pdf.
type Put k f = forall a. k a -> a -> f ()

-- | A build task is a stateful computation in a monadic context @f@ that is
-- given two callbacks: for reading and writing values to a key/value store.
type Task k a = forall f. Monad f => Get k f -> Put k f -> f a

-- | A unique task identifier, e.g. the path to the corresponding build script.
type TaskName = String

-- | A task along with its unique identifier.
data NamedTask k = NamedTask { taskName :: TaskName, task :: Task k () }

-- | A collection of build tasks using the same read and write interface.
type Tasks k = [NamedTask k]

-- | An example type of "black box" build tasks: we can only find out what they
-- read and write by executing them in a monadic context.
type BlackBox = Task Key ()

-- | Multiple black boxes, e.g. a collection of build scripts lying around.
type BlackBoxes = Tasks Key

-- | An example collection of black boxes.
tasks :: BlackBoxes
tasks = [NamedTask "release" release, NamedTask "build" build]
-- Placing "release" after "build" avoids restarting the "release" task:
-- [NamedTask "build" build, NamedTask "release" release]

-- | A typical build script that compiles a couple of C files, possibly
-- depending on some header files, and then links the resulting objects into an
-- executable.
build :: BlackBox
build get put = do
    compile "src/a.c" "obj/a.o" get put
    compile "src/b.c" "obj/b.o" get put
    link "obj" "out/exe" get put

-- | A script for packaging the contents of the directory @out@ in an archive.
-- Note that if called prematurely, it will miss some of the release files and
-- will /succeed/, yielding an incomplete archive. The task will therefore need
-- to be rerun whenever the key @Dir "out"@ is updated.
release :: BlackBox
release get put = do
    files   <- map ("out/" ++) <$> get (Dir "out")
    archive <- concat <$> mapM (get . File) files
    put (File "release.tar") archive

-- Note: this task doesn't need to be monadic, a selective interface is enough!
-- | Compile a C source file, possibly including the @lib.h@ header.
compile :: FilePath -> FilePath -> BlackBox
compile src obj get put = do
    source <- get (File src)
    header <- if "#include <lib.h>" `isInfixOf` source -- find an #include
         then do
            path <- get (Env "LIBDIR")
            get (File $ path ++ "/lib.h")
         else return ""
    put (File obj) (header ++ source)

-- | Link object files in a given directory, producing an executable. Note that
-- this task can /fail/ if run prematurely i.e. when some object files have not
-- yet been placed in the @obj@ directory, since some symbols will be undefined.
link :: FilePath -> FilePath -> BlackBox
link dir exe get put = do
    objs   <- map ("obj/" ++) <$> get (Dir dir)
    binary <- concat <$> mapM (get . File) objs
    put (File exe) binary

-- | A task execution log entry, recording either a read from a key and the
-- obtained value, or a write to a key, along with the written value.
data LogEntry k where
    GetEntry :: k a -> a -> LogEntry k
    PutEntry :: k a -> a -> LogEntry k

-- | A log is a sequence of log entries, in the execution order.
type Log k = [LogEntry k]

-- TODO: Can we simplify the implementation?
-- | Check if a log contains a 'GetEntry' for a given 'Key'. Useful to detect if
-- a task has a certain input dependency.
hasWrongGet :: Log Key -> Key a -> a -> Bool
hasWrongGet log k a = case k of
    File x -> any (matchesFile x a) log
    Env  x -> any (matchesEnv  x a) log
    Dir  x -> any (matchesDir  x a) log
  where
    matchesFile :: FilePath -> String -> LogEntry Key -> Bool
    matchesFile x a (GetEntry (File y) b) = (x == y) && (a /= b)
    matchesFile _ _ _ = False
    matchesEnv :: Variable -> String -> LogEntry Key -> Bool
    matchesEnv x a (GetEntry (Env y) b) = (x == y) && (a /= b)
    matchesEnv _ _ _ = False
    matchesDir :: FilePath -> [FilePath] -> LogEntry Key -> Bool
    matchesDir x a (GetEntry (Dir y) b) = (x == y) && (a /= b)
    matchesDir _ _ _ = False

{- Example execution of task 'build' in GHCi:

> log <- execute getIO putIO build
Get : File "src/a.c" = a
Put : File "obj/a.o" = a
Get : File "src/b.c" = b...#include <lib.h>...
Get : Env "LIBDIR" = libs
Get : File "libs/lib.h" = lib...
Put : File "obj/b.o" = lib...b...#include <lib.h>...
Get : Dir "obj" = ["a.o", "b.o", "c.o"]
Get : File "a.o" = 123
Get : File "b.o" = 456
Get : File "c.o" = 789
Put : File "out/exe" = 123456789

> log
[ Get (File "src/a.c", "a")
, Put (File "obj/a.o", "a")
, Get (File "src/b.c", "b...#include <lib.h>...")
, Get (Env "LIBDIR", "libs")
, Get (File "libs/lib.h", "lib...")
, Put (File "obj/b.o", "lib...b...#include <lib.h>...")
, Get (Dir "obj", ["a.o","b.o","c.o"])
, Get (File "a.o", "123")
, Get (File "b.o", "456")
, Get (File "c.o", "789")
, Put (File "out/exe", "123456789") ]

-}

-- TODO: Note that at the moment logging does not account for modifying
-- directories when creating new files.
-- | Execute a monadic task using given callbacks 'Get' and 'Put', logging all
-- reads and writes.
execute :: forall m k. Monad m => Get k m -> Put k m -> Task k () -> m (Log k)
execute get put task = execWriterT $ task loggingGet loggingPut
  where
    loggingGet :: k a -> WriterT (Log k) m a
    loggingGet k = do
        a <- lift $ get k
        tell [GetEntry k a]
        return a
    loggingPut :: k a -> a -> WriterT (Log k) m ()
    loggingPut k a = do
        lift $ put k a
        tell [PutEntry k a]

-- | An association of keys to values.
newtype Store = Store { getValue :: forall a. Key a -> a }

putValue :: Key a -> a -> Store -> Store
putValue (File x) a (Store f) = Store $ \k -> case k of
    File y | x == y -> a
    _ -> f k
putValue (Env x) a (Store f) = Store $ \k -> case k of
    Env y | x == y -> a
    _ -> f k
putValue (Dir x) a (Store f) = Store $ \k -> case k of
    Dir y | x == y -> a
    _ -> f k

-- | An example store with the following contents:
--
-- File "src/a.c"    -> "a"
-- File "src/b.c"    -> "b...#include <lib.h>..."
-- File "obj/main.o" -> "...main..."
-- File "lib/lib.h"  -> "lib..."
-- File "out/README" -> "This is a README..."
-- Env "LIBDIR"      -> "lib"
-- Dir "obj"         -> ["main.o"]
-- Dir "out"         -> ["README"]
exampleStore :: Store
exampleStore = Store $ \case
    File "src/a.c"    -> "a"
    File "src/b.c"    -> "b...#include <lib.h>..."
    File "obj/main.o" -> "...main..."
    File "lib/lib.h"  -> "lib..."
    File "out/README" -> "This is a README..."
    Env "LIBDIR"      -> "lib"
    Dir "obj"         -> ["main.o"]
    Dir "out"         -> ["README"]

    File _ -> "<empty file>"
    Env  _ -> "<empty variable>"
    Dir  _ -> [] -- empty directory

-- | Known information about build task dependencies.
type Graph = TaskName -> Maybe (Log Key)

-- | A build system that builds a collection of black box tasks by executing
-- them blindly, and recording the resulting dependencies.
blindBuild :: BlackBoxes -> Store -> Graph -> (Store, Graph)
blindBuild tasks store graph = fst $ execState build ((store, graph), tasks)
  where
    build :: State ((Store, Graph), BlackBoxes) ()
    build = do
      queue <- gets snd
      case queue of
        [] -> return ()
        (NamedTask id task : tasks) -> do
          -- Remove the first task from the queue
          modify $ \((s, g), _) -> ((s, g), tasks)
          -- Execute the task, possibly restaring some previously executed tasks
          log <- trace ("Execute " ++ id) $ execute get put task
          modify $ \((s, g), ts) -> let ng t = if t == id then Just log else g t
                                   in ((s, ng), ts)
          -- Build the rest of the queue
          build

    -- Simply return whatever is in the store.
    get :: Key a -> State ((Store, Graph), BlackBoxes) a
    get k = do
        store <- gets (fst . fst)
        let a = getValue store k
        trace ("Get (" ++ showKey k ++ ", " ++ showValue k a ++ ")") $ return a

    -- Update the value, and restart any tasks which depended on it but have
    -- been executed too early.
    put :: Key a -> a -> State ((Store, Graph), BlackBoxes) ()
    put k a = do
        -- Update the store
        trace ("Put (" ++ showKey k ++ ", " ++ showValue k a ++ ")") $
            modify $ \((s, g), bs) -> ((putValue k a s, g), bs)
        -- Restart any tasks which depended on this key
        graph <- gets (snd . fst)
        queue <- gets snd
        forM_ tasks $ \task@(NamedTask id _) -> case graph id of
            Nothing  -> return ()
            Just log -> when (hasWrongGet log k a && id `notInQueue` queue) $
                trace ("Restart " ++ id) $
                    modify $ \((s, g), bs) -> ((s, g), bs ++ [task])
        -- Make sure to update the corresponding directory key if a new file has
        -- been created
        case k of
            File path -> do
                let dir  = takeDirectory path
                    file = takeFileName path
                store <- gets (fst . fst)
                let files = getValue store (Dir dir)
                when (file `notElem` files) $ put (Dir dir) (files ++ [file])
            _ -> return ()

    -- Check that a task does not appear in a queue
    notInQueue :: TaskName -> BlackBoxes -> Bool
    notInQueue _  []     = True
    notInQueue id (t:ts) | id == taskName t = False
                         | otherwise        = notInQueue id ts

{- Example blind build

> res = blindBuild tasks exampleStore (const Nothing)
> res `seq` ()

Execute release
Get (Dir "out", ["README"])
Get (File "out/README", "This is a README...")
Put (File "release.tar", "This is a README...")
Put (Dir ".", ["release.tar"])
Execute build
Get (File "src/a.c", "a")
Put (File "obj/a.o", "a")
Put (Dir "obj", ["main.o","a.o"])
Get (File "src/b.c", "b...#include <lib.h>...")
Get (Env "LIBDIR", "lib")
Get (File "lib/lib.h", "lib...")
Put (File "obj/b.o", "lib...b...#include <lib.h>...")
Put (Dir "obj", ["main.o","a.o","b.o"])
Get (Dir "obj", ["main.o","a.o","b.o"])
Get (File "obj/main.o", "...main...")
Get (File "obj/a.o", "a")
Get (File "obj/b.o", "lib...b...#include <lib.h>...")
Put (File "out/exe", "...main...alib...b...#include <lib.h>...")
Put (Dir "out", ["README","exe"])
Restart release
Execute release
Get (Dir "out", ["README","exe"])
Get (File "out/README", "This is a README...")
Get (File "out/exe", "...main...alib...b...#include <lib.h>...")
Put (File "release.tar", "This is a README......main...alib...b...#include <lib.h>...")

> snd res "release"

Just [ Get (Dir "out", ["README","exe"])
     , Get (File "out/README", "This is a README...")
     , Get (File "out/exe", "alib...b...#include <lib.h>...<empty file>")
     , Put (File "release.tar", "This is a README...alib...b...#include <lib.h>...<empty file>")]

> snd res "build"

Just [ Get (File "src/a.c", "a")
     , Put (File "obj/a.o", "a")
     , Get (File "src/b.c", "b...#include <lib.h>...")
     , Get (Env "LIBDIR", "lib")
     , Get (File "lib/lib.h", "lib...")
     , Put (File "obj/b.o", "lib...b...#include <lib.h>...")
     , Get (Dir "obj", ["main.o","a.o","b.o"])
     , Get (File "obj/main.o", "...main...")
     , Get (File "obj/a.o", "a")
     , Get (File "obj/b.o", "lib...b...#include <lib.h>...")
     , Put (File "out/exe", "...main...alib...b...#include <lib.h>...")]

-}

---------------------------- Some boilerplate code -----------------------------

-- | A way to show the name of a key.
type ShowKey k = forall a. k a -> String

-- | A simple pretty-printer for the data type 'Key'.
showKey :: ShowKey Key
showKey (File f) = "File " ++ show f
showKey (Env  v) = "Env "  ++ show v
showKey (Dir  d) = "Dir "  ++ show d

-- | Show a value corresponding to a key, extracting an appropriate 'Show'
-- instance from it.
showValue :: Key a -> a -> String
showValue (File _) f = show f
showValue (Env  _) v = show v
showValue (Dir  _) d = show d

instance Show (LogEntry Key) where
    show (GetEntry k@(File _) a) = "Get (" ++ showKey k ++ ", " ++ show a ++ ")"
    show (GetEntry k@(Env  _) a) = "Get (" ++ showKey k ++ ", " ++ show a ++ ")"
    show (GetEntry k@(Dir  _) a) = "Get (" ++ showKey k ++ ", " ++ show a ++ ")"

    show (PutEntry k@(File _) a) = "Put (" ++ showKey k ++ ", " ++ show a ++ ")"
    show (PutEntry k@(Env  _) a) = "Put (" ++ showKey k ++ ", " ++ show a ++ ")"
    show (PutEntry k@(Dir  _) a) = "Put (" ++ showKey k ++ ", " ++ show a ++ ")"

----------------------------- Auxiliary functions ------------------------------

-- | A 'Get' in 'IO' for GHCi experiments.
getIO :: Get Key IO
getIO (File f) = putStr ("Get : File " ++ show f ++ " = ") >> getLine
getIO (Env  v) = putStr ("Get : Env " ++ show v ++ " = ") >> getLine
getIO (Dir  d) = putStr ("Get : Dir " ++ show d ++ " = ") >> (read <$> getLine)

-- | A 'Put' in 'IO' for GHCi experiments.
putIO :: Put Key IO
putIO (File f) x = putStr ("Put : File " ++ show f ++ " = ") >> putStrLn x
putIO (Env  v) x = putStr ("Put : Env " ++ show v ++ " = ") >> putStrLn x
putIO (Dir  d) x = putStr ("Put : Dir " ++ show d ++ " = ") >> print x
