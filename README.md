# Untitled Functional Language

A minimal lazy functional programming language inspired by System F and ML.

# Introduction

This project aims to explore what it takes to build a programming language from the ground
up. It is therefore designed to be as small as possible to be useful. For this reason, it's
missing many features you'd expect in a full featured programming language, such as 
user-defined types and reusable modules.

# Features

Untitled Functional Language is roughly equivalent to System F, although it has 
extensions for convenience, such as:

 * Named functions
 * Local let-bindings

It contains the following built-in types:

 * Boolean
 * Char
 * Integer
 * Float
 * String

Expressions are evaluated lazily in non-strict order.

The syntax is based on ML with Hindley-Milner type inference. Polymorphic types are
therefore limited to rank-1.

# Syntax

A program contains a sequence of named functions, including main:

    main : ()
    main = greet "Hello, World!"

    greet : String -> ()
    greet msg = println msg
