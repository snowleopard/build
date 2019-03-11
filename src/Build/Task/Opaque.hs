{-# LANGUAGE ConstraintKinds, RankNTypes, FlexibleInstances, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Opaque monadic tasks, whose inputs and outputs can be dynamic.
module Build.Task.Opaque where

import Control.Monad.Writer
import Data.List

-- | A collection of keys for accessing files, environment variables, and
-- contents of directories.
data Key a where
    File :: FilePath -> Key String     -- ^ File contents.
    Env  :: String   -> Key String     -- ^ Environment variable.
    Dir  :: FilePath -> Key [FilePath] -- ^ Directory contents.

-- | Read a value from a key in a computation context @f@.
type Get k f = forall a. k a -> f a

-- | Write a value to a key in a computation context @f@. Note: the type should
-- be changed to @forall a. k a -> f a -> f a@ to allow for static analysis of
-- applicative and selective build tasks, since we cannot have @a@ in a static
-- context @f@, e.g. in @Const@. See more details in Section 5.3 of this paper:
-- https://www.staff.ncl.ac.uk/andrey.mokhov/selective-functors.pdf.
type Put k f = forall a. k a -> a -> f ()

-- | A build task is a stateful computation in a monadic context @f@ that is
-- given two callbacks: for reading and writing values to a key/value store.
type Task k a = forall f. Monad f => Get k f -> Put k f -> f a

-- | A collection of build tasks using the same read and write interface.
type Tasks k = forall f. Monad f => Get k f -> Put k f -> [f ()]

-- | An example type of "black box" build tasks: we can only find out what they
-- read and write by executing them in a monadic context.
type BlackBox = Task Key ()

-- | Multiple black boxes, e.g. a collection of build scripts lying around.
type BlackBoxes = Tasks Key

-- | An example collection of black boxes.
tasks :: BlackBoxes
tasks get put = [build get put, release get put]

-- | A typical build script that compiles a couple of C files, possibly
-- depending on some header files, and then links the resulting objects into an
-- executable.
build :: BlackBox
build get put = do
    compile "src/a.c" "obj/a.o" get put
    compile "src/b.c" "obj/b.o" get put
    link "obj" "release/exe" get put

-- | A script for packaging the contents of the @release@ directory in an
-- archive. Note that if called prematurely, it will find no files in the
-- directory and /succeed/ by producing an empty archive. The task will
-- therefore need to be rerun whenever the key 'Dir' is updated.
release :: BlackBox
release get put = do
    files   <- get (Dir "release")
    archive <- concat <$> mapM (get . File) files
    put (File "release.tar") archive

-- Note: this task doesn't need to be monadic, a selective interface is enough!
-- | Compile a C source file, possibly including the @lib.h@ header.
compile :: FilePath -> FilePath -> BlackBox
compile src obj get put = do
    source <- get (File src)
    header <- if ("#include <lib.h>" `isInfixOf` source)
         then do
            path <- get (Env "LIBPATH")
            get (File $ path ++ "/lib.h")
         else return ""
    put (File obj) (header ++ source)

-- | Compile object files in a given directory, producing an executable. Note
-- that this task can /fail/ if run prematurely i.e. when no object files exist,
-- since some symbols will be undefined.
link :: FilePath -> FilePath -> BlackBox
link dir exe get put = do
    objs   <- get (Dir dir)
    binary <- concat <$> mapM (get . File) objs
    put (File exe) binary

-- | A way to show the name of a key.
type ShowKey k = forall a. k a -> String

-- | A simple pretty-printer for the data type 'Key'.
showKey :: ShowKey Key
showKey (File f) = "File " ++ show f
showKey (Env  v) = "Env "  ++ show v
showKey (Dir  d) = "Dir "  ++ show d

-- | A task execution log entry, recording either a read from a key and the
-- obtainedd value, or a write to a key, along with the written value.
data LogEntry k where
    GetEntry :: k a -> a -> LogEntry k
    PutEntry :: k a -> a -> LogEntry k

instance Show (LogEntry Key) where
    show (GetEntry k@(File _) a) = "Get (" ++ showKey k ++ ", " ++ show a ++ ")"
    show (GetEntry k@(Env  _) a) = "Get (" ++ showKey k ++ ", " ++ show a ++ ")"
    show (GetEntry k@(Dir  _) a) = "Get (" ++ showKey k ++ ", " ++ show a ++ ")"

    show (PutEntry k@(File _) a) = "Put (" ++ showKey k ++ ", " ++ show a ++ ")"
    show (PutEntry k@(Env  _) a) = "Put (" ++ showKey k ++ ", " ++ show a ++ ")"
    show (PutEntry k@(Dir  _) a) = "Put (" ++ showKey k ++ ", " ++ show a ++ ")"

-- | A log is a sequence of log entries, in the execution order.
type Log k = [LogEntry k]

{- Example execution of task 'build' in GHCi:

> log <- execute getIO putIO build
Get : File "src/a.c" = a
Put : File "obj/a.o" = a
Get : File "src/b.c" = b...#include <lib.h>...
Get : Env "LIBPATH" = libs
Get : File "libs/lib.h" = lib...
Put : File "obj/b.o" = lib...b...#include <lib.h>...
Get : Dir "obj" = ["a.o", "b.o", "c.o"]
Get : File "a.o" = 123
Get : File "b.o" = 456
Get : File "c.o" = 789
Put : File "release/exe" = 123456789

> log
[ Get (File "src/a.c", "a")
, Put (File "obj/a.o", "a")
, Get (File "src/b.c", "b...#include <lib.h>...")
, Get (Env "LIBPATH", "libs")
, Get (File "libs/lib.h", "lib...")
, Put (File "obj/b.o", "lib...b...#include <lib.h>...")
, Get (Dir "obj", ["a.o","b.o","c.o"])
, Get (File "a.o", "123")
, Get (File "b.o", "456")
, Get (File "c.o", "789")
, Put (File "release/exe", "123456789") ]

-}

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

getIO :: Get Key IO
getIO (File f) = putStr ("Get : File " ++ show f ++ " = ") >> getLine
getIO (Env  v) = putStr ("Get : Env " ++ show v ++ " = ") >> getLine
getIO (Dir  d) = putStr ("Get : Dir " ++ show d ++ " = ") >> (read <$> getLine)

putIO :: Put Key IO
putIO (File f) x = putStr ("Put : File " ++ show f ++ " = ") >> (putStrLn x)
putIO (Env  v) x = putStr ("Put : Env " ++ show v ++ " = ") >> (putStrLn x)
putIO (Dir  d) x = putStr ("Put : Dir " ++ show d ++ " = ") >> (putStrLn $ show x)

{- An older example discussed in https://github.com/snowleopard/build/issues/19.

data Key a where
    Dir  :: FilePath -> Key [(FilePath, Int)] -- We need to depend on index
    File :: FilePath -> Key String
    T    :: FilePath -> Key (Task Monad Key a)
    Let  :: Task Monad Key a -> Key a

compileAllCFiles :: Task Monad Key ()
compileAllCFiles get put = do
    srcs <- get (Dir "src/c/")
    void $ traverse (\src -> compileC src get put) srcs -- independent/parallel

compileC :: (FilePath, Int) -> Task Monad Key ()
compileC (cFile, index) get put = do
    let objFile = cFile ++ ".o"
    src  <- get (File cFile)
    deps <- traverse (get . File) (cDependencies src)
    void $ put (File objFile) (pure $ compile src index deps)
  where
    cDependencies _src        = []  -- insert dependency analysis here
    compile src _index _deps  = src -- insert a C compiler here
-}
