{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ... 
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
  getArgs >>= \a -> 
  case a of
    Nil -> 
      putStrLn " no args"
    h:._ -> 
      run h


type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
--run name = (readFile name) >>= (\c -> ( getFiles (lines c) ) >>= \z -> printFiles(z) )

run name =
  do c <- readFile name
     z <- getFiles (lines c)
     printFiles z


 --void (getFiles (lines  readFile 

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles = sequence . (<$>) getFile
    --       sequence ( (<$>) getFile)

getFile ::
  FilePath
  -> IO (FilePath, Chars)
-- getFile name = lift2 (\x y-> (x, y) ) (IO ( name )) (readFile name)
getFile name = lift2 (\x y-> (x, y) ) (pure name) (readFile name)

-- getFile name = (\c -> (name,c) <$> readFile name)
-- getFile name = ((,) name ) <$> readFile name)

printFiles ::
  List (FilePath, Chars)
  -> IO ()
--printFiles l = void (sequence ( (\(x,y) -> printFile x y ) <$> l ))

printFiles =  void . sequence . (<$>) (uncurry printFile)

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile name contents =
  do 
    putStrLn name 
    putStrLn contents

