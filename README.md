# Haskshell

Haskshell is a new terminal shell powered by GHCi and Haskell. It was created to
provide an interactive environment for Haskell programming that is both powerful
and easy to use and has all the benefits of Haskell such as lazy evaluation,
partial function application, and much more. Haskshell is free and open-source.

![Haskshell Preview](https://raw.githubusercontent.com/Archaversine/Haskshell/main/imgs/preview.png)

## Features

Haskshell currently offers the following features:

- Integration with GHCi: Haskshell uses GHCi as its backend, which means that it
  provides all of the functionality of GHCi, including the ability to run
  Haskell code, interact with the Haskell runtime, and even debug Haskell code.

- Fugue module: Haskshell comes with a module called Fugue, which contains all
  of the main commands used by Haskshell. This module is named to be a play on
  words with Prelude, the standard Haskell library.

- Passacaglia module: Haskshell also comes with a module called Passacaglia,
  another play on words, that is meant specifically for the user's custom
  functions and bindings. This module is automatically generated by Haskshell if
  it doesn't exist. Anything inside this module is directly imported into the
  next Haskshell session.

- Git bindings: Haskshell has bindings for basic git commands, making it easy to
  work with Git repositories from within the shell.

- Color module: Haskshell has its own text formatting module called Color, which
  allows users to easily add color to their output.

- Wiki pages: The Haskshell GitHub Wiki has a wiki page for every Haskshell
  command, with a definition, syntax, and example for each command.

## Compared To Bash

Haskell syntax is more expressive and powerful than Bash syntax, allowing users
to write complex commands more easily. Haskell offers a wide range of language
features, including type safety, pattern matching, and higher-order functions,
which can make it easier to work with complex data structures and algorithms.

Like many things in bash, a for loop doesn't have the best syntax. For example
here's how one would write a for loop to iterate through all files given by ls
and print them one by one:

``` bash
for file in $(ls); do
    echo $file
done
```

And here's how it would be done in Haskshell:

``` haskell
lsstr >>= mapM_ echo
```

But for a more literal translation, here's an unsimplifed version:

``` haskell
lsstr >>= \files -> mapM_ (\file -> echo file) files
```

And due to the nature of Haskell, it can even be split into different functions
(Note that `echo` is an alias for `putStrLn`):

``` haskell
printFile f = echo f
printFiles files = mapM_ printFile files

lsstr >>= printFiles
```

## Getting started

To get started with Haskshell, users can download the latest version from the
repository (make sure ghci is installed). Once installed, they can launch
Haskshell from the command line and begin using it immediately.

Users can also customize their Haskshell experience by modifying the run.ghci
file, which is located in the project's root directory or the Passacaglia.hs
file also located in the root directory. The configuration file provides a way
to customize the prompt, set up aliases for common commands, and define custom
commands.

## Documentation

The Haskshell Git Wiki has information about all of the currently implemented
functions in Haskshell from Fugue. Each page has information about what the
command does, what it's syntax and type signature looks like, and even gives an
example of how it can be used. The wiki is available
[here](https://github.com/Archaversine/Haskshell/wiki/Haskshell).

## Community

Haskshell is a relatively new project, but it is free and open-source. Users are
encouraged to contribute to the project by submitting bug reports, feature
requests, and pull requests.
