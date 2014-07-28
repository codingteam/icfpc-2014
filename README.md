codingteam ICFP Programming Contest 2014 submission
===================================================

We've prepared Scheme-like Lisp dialect for GCC and have written the compiler in
Haskell.

Ghost code included is taken from sample because we haven't still managed to
make our own ghost :(

(Ghost walker sample placed in the `ghc` branch but it was ready to late to be
submitted.)

Building the project
--------------------

    $ cd gcc
		$ cabal install --only-dependencies
		$ cabal build
		$ .\dist\build\gcc-compiler\gcc-compiler.exe compile .\waveman.lisp waveman.gcc

Participants
------------

- @portnov
- @ForNeVeR
- @rexim
- @Minoru

Details
--------

team name: codingteam

team contact email: fvnever@gmail.com

team programming language: Haskell

team homepage: https://github.com/codingteam/
