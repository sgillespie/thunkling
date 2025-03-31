# Untitled Function Language

A minimal lazy functional programming language inspired by System F and ML.

# Introduction

This project aims to explore what it takes to build a programming language from the ground
up. It is therefore designed to be as small as possible but big enough to be useful. It
lacks features found in most other languages such as user-defined types.

Untitled Functional Language is roughly equivalent to System F, along with the following
extensions:

 * Named global functions
 * Local let-bindings
 * Built-in types (Boolean, Integer, Float, Char, and String)
 * Literals

# Syntax

A UFL file consists of a sequence of global bindings and (optionally) their type
annotations:

    f : Int
    f = 10

    g : Bool -> Bool
    g x = x

    h : forall a. a -> a
    h x = x

    main : ()
    main = ()


