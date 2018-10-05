# Notes for CS 161

### Table of Contents
DATE | AIM
:---:| ---
10/1 | [Introduction](#100118-introduction)
10/3 | [Lists](#100318-lists)
10/5 | [Types](#100518-types)

### 10.05.18 Types
- defining a new type
- ```type {- alias -} String = [Char]```
```Haskell

-- type {- alias -} String = [Char] <- already defined in Prelude

data Person
  = Student String String Int
  | Instructor String [String]
  | Parent Int
  deriving (Show)

--  [Student, Instructor, Instructor] works because they are the same type

getName :: Person -> String
getName (Student name _ _) = name
-- use data constructors to pattern match values (underscores are like wildcards)
getName (Instructor name _) = name
-- getName (Instructor "ravi" []) -> "ravi"
-- map getName [Instructor "Ravi" [], Student  "Christian" "asdafsa" 100000] -> ["Ravi", "Christian"]
-- getName (Parent _) = "No Name"

```
- How to treat smth like Parent that has no way of getting name?
  - define another MaybeError type
```Haskell
data MaybeError
  = Error String -- "error"
  | Success String
```



### 10.03.18 Lists

- tuple syntax to define packages of items, denoted by parentheses
- you can have empty tuple ("unit"), two-tuple is "pair", etc..
- no such thing as a one-element tuple, because that would just be the item in parentheses (e.g. ((((True)))) :: Bool)
- a list on the other hand can have an unknown number of elements
  - ```:t [True, False, False] :: [Bool]``` <- list of booleans
  - every element must have the same type
- strings are lists of characters: "abc" is syntactic sugar for ```['a', 'b', 'c']```
  - ```"abc" :: [Char]```
  - we could say ```type String = [Char]``` and "abc" would be type String
- Haskell has 2 ways of building a list
  1. the empty list: []
  2. start with the empty list and construct a new list by attaching a new element  (you can only prepend lists)
    - ```1 : []``` -> ```[1]```
    - ```1 : (2 : [])``` -> ```[1, 2]```
- you can omit parens: ```1 : 2 : 3 : []```, Haskell again implicitly adds parens to the right
- type of empty list
  - ```:t [] :: {- forall a. -} [a]``` <- how it should be read
- ```id :: a -> a```
  - polymorphism 
- defining a length fxn
  - ```length :: {- for all a. -} [a] -> Int```
  - ```length [] = 0```
  - ```length (x:xs) = 1 + length xs``` <- x:xs is list pattern, x gets mapped to single element, xs gets mapped to rest
- read about map to more generally define list fxns
- ```map :: (Int -> Bool) -> [Int] -> [Bool]
- you can use _ if you don't want to give something a name
    
  

### 10.01.18 Introduction

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
- misc:
```
-- hoursPerDay = 24
-- minutesPerHour = 60
-- hoursPerDay = 30


minutesPerDay =
  let hoursPerDay = 24 in
  let minutesPerHour = 60 in
  hoursPerDay * minutesPerHour
```
- nested lets allow you to shadow whereas top level file gives you error
