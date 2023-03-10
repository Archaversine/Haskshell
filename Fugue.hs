{-# LANGUAGE FlexibleInstances #-}

module Fugue where

import Color

import Data.List ( isInfixOf, isPrefixOf )

import Control.Monad ( forM_, filterM )
import Control.Exception ( try )

import System.Directory
import System.Process
import System.FilePath ( (</>) )

import qualified Git

class CommandArgument command where
  toArgArray :: command -> [String]

instance CommandArgument String where
  toArgArray x = [x]

instance CommandArgument [String] where
  toArgArray = id

pwd :: IO ()
pwd = getCurrentDirectory >>= putStrLn

pwdstr :: IO String
pwdstr = getCurrentDirectory

lsPredicate :: String -> Bool
lsPredicate = (`notElem` [".", ".."])

lsNoHidden :: String -> Bool
lsNoHidden = not . isPrefixOf "."

isBroken :: FilePath -> IO Bool
isBroken path = do
  result <- try (getPermissions path) :: IO (Either IOError Permissions)
  case result of
    Right _ -> return False
    Left _ -> return True

lsPrint :: Foldable t => FilePath -> t FilePath -> IO ()
lsPrint currentDir files = do
  forM_ files $ \file -> do
    let path = currentDir </> file
    isFile <- doesFileExist path
    broken <- isBroken path

    p <- if broken then return emptyPermissions else getPermissions path

    let filePrefix = if isFile then Reset else LightBlue

    putStrLn $ "("
      ++ (if isFile then "F " else "D " % LightBlue)
      ++ (if readable p   then "r" % Green else "-")
      ++ (if writable p   then "w" % Yellow else "-")
      ++ (if executable p then "x" % Red else "-")
      ++ ") "
      ++ (if not broken then file % filePrefix else file % [Red, Blink])
      ++ "\x1b[0m"

ls :: IO ()
ls = do
  currentDir <- getCurrentDirectory
  contents <- getDirectoryContents currentDir
  lsPrint currentDir $ filter lsPredicate . filter lsNoHidden $ contents

lsstr :: IO [String]
lsstr = do
  currentDir <- getCurrentDirectory
  contents <- getDirectoryContents currentDir
  return $ filter lsPredicate . filter lsNoHidden $ contents

lsh :: IO ()
lsh = do
  currentDir <- getCurrentDirectory
  contents <- getDirectoryContents currentDir
  lsPrint currentDir $ filter lsPredicate contents

lshstr :: IO [String]
lshstr = do
  currentDir <- getCurrentDirectory
  contents <- getDirectoryContents currentDir
  return $ filter lsPredicate contents

peek :: FilePath -> IO ()
peek path = do
  contents <- getDirectoryContents path
  lsPrint path $ filter lsPredicate . filter lsNoHidden $ contents

peekstr :: FilePath -> IO [String]
peekstr path = do
  contents <- getDirectoryContents path
  return $ filter lsPredicate . filter lsNoHidden $ contents

peekh :: FilePath -> IO ()
peekh path = do
  contents <- getDirectoryContents path
  lsPrint path $ filter lsPredicate contents

peekhstr :: FilePath -> IO [String]
peekhstr path = do
  contents <- getDirectoryContents path
  return $ filter lsPredicate contents

cd :: FilePath -> IO ()
cd = setCurrentDirectory

clear :: IO ()
clear = putStr "\x1b[2J\x1b[H"

grep :: String -> [String] -> IO ()
grep phrase elems = do
  filtered <- filterM (return . isInfixOf phrase) elems
  mapM_ putStrLn filtered

grepstr :: String -> [String] -> [String]
grepstr phrase = filter (isInfixOf phrase)

ngrep :: String -> [String] -> IO ()
ngrep phrase elems = do
  filtered <- filterM (return . not . isInfixOf phrase) elems
  mapM_ putStrLn filtered

ngrepstr :: String -> [String] -> [String]
ngrepstr phrase = filter (not . isInfixOf phrase)

touch :: CommandArgument command => command -> IO ()
touch = mapM_ (`writeFile` "") . toArgArray

cat :: FilePath -> IO ()
cat filename = do
  contents <- readFile filename
  putStrLn contents

catstr :: FilePath -> IO String
catstr = readFile

less :: String -> IO ()
less text = callCommand $ "less <<< \"" ++ text ++ "\""

mkdir :: CommandArgument command => command -> IO ()
mkdir pathArg = mapM_ createDirectory $ toArgArray pathArg

rm :: CommandArgument command => command -> IO ()
rm = mapM_ removeFile . toArgArray

rmdir :: CommandArgument command => command -> IO ()
rmdir = mapM_ removeDirectory . toArgArray

rrmdir :: CommandArgument command => command -> IO ()
rrmdir = mapM_ removeDirectoryRecursive . toArgArray

cp :: FilePath -> FilePath -> IO ()
cp source dest = readFile source >>= writeFile dest

mv :: FilePath -> FilePath -> IO ()
mv source dest = readFile source >>= writeFile dest >> removeFile source

vim :: FilePath -> IO ()
vim path = callCommand $ "vim " ++ path

emacs :: FilePath -> IO ()
emacs path = callCommand $ "emacs " ++ path

chmod :: FilePath -> Bool -> Bool -> Bool -> IO ()
chmod fileName allowRead allowWrite allowExecute = do
  p <- getPermissions fileName
  setPermissions fileName (p {readable = allowRead, writable = allowWrite, executable = allowExecute})
