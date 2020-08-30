# BlockChain

A simple example of how to make a block chain.

See the [https://github.com/jeroanan/BlockChain|blog post] that gives a
tutorial relating to this repo.

## Pre-requisites

A couple of packages need to be installed from PlaneT to compile this project:

1. sha
2. predicates

## The code

An overview of the files:

1. main.rkt -- the entrypoint into the program
2. chain.rkt -- contains the representation of the chain as a whole
3. block.rkt -- contains the deifnition of a block
4. member.rkt -- contains a macro that generates fields with getters/setters
5. gui/mainwindow.rkt -- Contains the definition of the window that appears
   when main.rkt is run.
