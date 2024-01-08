module Git where

import System.Process

-- Initialize git repo
init :: IO ()
init = callCommand "git init"

-- Print status of git repo
status :: IO ()
status = callCommand "git status"

-- Fetch from online repo
fetch :: IO ()
fetch = callCommand "git fetch"

-- Add untracked file to repo
add :: [Char] -> IO ()
add path = callCommand $ "git add " ++ path

-- List all branches
branch :: IO ()
branch = callCommand "git branch"

-- List all remote branches
remoteBranches :: IO ()
remoteBranches = callCommand "git branch -r"

-- Rename current branch
setCurrentBranchName :: [Char] -> IO ()
setCurrentBranchName name = callCommand $ "git branch -m " ++ name

-- Rename a specific branch
setBranchName :: [Char] -> [Char] -> IO ()
setBranchName oldName newName = callCommand $ "git branch -m " ++ oldName ++ " " ++ newName

-- Checkout a branch
checkout :: [Char] -> IO ()
checkout branchName = callCommand $ "git checkout " ++ branchName

checkoutNew :: [Char] -> IO ()
checkoutNew branchName = callCommand $ "git checkout -b " ++ branchName

-- Make a commit with a message
commit :: [Char] -> IO ()
commit message = callCommand $ "git commit -m \"" ++ message ++ "\""

-- Push to online repo
push :: IO ()
push = callCommand "git push"

-- Push & set upstream
pushU :: [Char] -> [Char] -> IO ()
pushU remote branchName = callCommand $ "git push -u " ++ remote ++ " " ++ branchName

-- Pull from online repo
pull :: IO ()
pull = callCommand "git pull"

-- Git merge
merge :: [Char] -> IO ()
merge from = callCommand $ "git merge " ++ from

-- Graph git commits
graph :: IO ()
graph = callCommand "git graph"

-- Clone git repository
clone :: [Char] -> IO ()
clone repo = callCommand $ "git clone " ++ repo
