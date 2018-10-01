# Notes for CS 161


### Introduction

- learning Haskell is useful for the way it forces you to think
- [Class Page](http://cmsc-16100.cs.uchicago.edu/2018-autumn/)

###### Basic Haskell
- ++ is concatenation operator
- statically typed
  - check type using :type
- types are more general
- typecast by using format ```:t (e :: T)```
  - e is meta variable (basically means check that e has type T)
- Can't typecast a more specific type to a more general type (e.g. Fractional to Int)
- prefix notation for functions
  - (+) 1 2 is 3
- every fxn in Haskell takes one argument and one output (S -> T)
  - but the output value can be another fxn: S -> (T -> (U -> V))
  - nest these to simulate multiple functions and eliminate the parentheses: S -> T -> U -> V
  - (((S -> T) -> U) -> V) is something different because parentheses are implicitly put on the right
- define variables using *let*
  - e.g. ```let one = 1```
  - first letter has to be lowercase
- variables are **immutable**
  - the word one will always equal 1
  - never be removed unless you quit
  - but you can bind another definition of one (e.g. ```let one = 11```)
  - most recent binding overshadows previous ones but the first one is still there 
- defining functions
  - ```let add1 y = 1 + y```
  - ```add1 11``` is 12
  - two arguments:
    - ```let add x y = x + y```
- function variable interaction
  - ```let one = 1```
  - ```let add1 y = one + y```
  - ```lett one = 11```
  - ```add1 2 is still 3``` because when we defined one, it was shorthand for the number 1
- loading files
```
:load Oct01
```
- comments using -- (single line) or {- -} (multiple line)
