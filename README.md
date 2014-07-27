codingteam ICFPC submission
===========================

We've prepared Scheme-like Lisp dialect for GCC and have written the compiler in
Haskell.

Ghost code included is taken from sample because we haven't still managed to
make our own ghost :(

Hopefully my teammates will submit new version with the ghost implementation.
~ F.

Building the project
--------------------

    $ cd gcc
		$ cabal install --only-dependencies
		$ cabal build
		$ .\dist\build\gcc-compiler\gcc-compiler.exe compile .\waveman.lisp waveman.gcc

Participants
------------

portnov

ForNeVeR

rexim

Minoru

Details
--------

team name: codingteam

team contact email: fvnever@gmail.com

team programming language: Haskell

team homepage: https://github.com/codingteam/
