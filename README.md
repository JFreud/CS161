# Notes for CS 161

### Table of Contents
DATE | AIM
:---:| ---
10/1 | [Introduction](#100118-introduction)
10/3 | [Lists](#100318-lists)
10/5 | [Types](#100518-types)
10/8 | [Functions](#100818-functions)
10/10 | [Type Classes](#101018-type-classes)
10/12 | [IO](#101218-io)
10/15 | [Misc](#101518-misc)
10/17 | [Maybe](#101718-maybe)
10/22 | [Applicative(#102218-applicative)


### 10.22.18 Applicative
```applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b```
- must have kind * -> *
```Haskell
class Functor t => Applicative t where
    apply :: t (a -> b) -> t a -> t b
    
instance Applicative (Maybe a) where
    apply (Just f) ma = fmap f ma
    apply Nothing   _ = Nothing
```
- applied lists:
```Haskell
instance Applicative_ [] where
    -- apply :: [(a, b)] -> [a] -> [b]
    -- apply fs as = concatMap (\f -> map f as) fs
    -- apply fs as = concatMap (\f -> concatMap (\a -> [f a]) as) fs
    apply fs as = [f a | f <- fs, a <- as]
```
- one instance definition per type, bc Haskell doesn't want the ambiguity
  - to get around this, define a newtype
- pure takes an unwrapped value and wraps it however it makes sense
```Haskell
<*> apply operator
[(+),(+)] <*> [1..3] <*> [1..3]
```
- functions can be added to the Functor class


### 10.17.18 Maybe
- remember maybe?
```Haskell
addOne :: Num n => Maybe n -> Maybe n
addOne (Just a) = Just (a + 1)
addOne Nothing  = Nothing

maybeLength :: Maybe [a] -> Maybe Int
maybeLength (Just xs) = Just (length xs)
maybeLength Nothing   = Nothing
```
- there's an easier way to do this
```Haskell
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing  = Nothing
mapMaybe f (Just x) = Just $ f x
```
- now:
```Haskell
addOne ns = mapMaybe (+1) ns
maybeLength ns = mapMaybe length ns
```
- more stuff I don't really understand
```Haskell
bar                           :: (t1 -> t2 -> t3)
mapMaybe {- t1 (t2->t3)-} bar :: Maybe t1 -> Maybe t2 -> t3
```
```Haskell
applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe (Just f) ma = mapMaybe f ma
applyMaybe _ _         = Nothing
```
- now you can ```mapMaybe (+) ) (Just 1) `applyMaybe` (Just 3)``` since the first part becomes a maybe function
- now you be lifting functions
```Haskell
lift2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
lift2 f ma b =
  mapMaybe f ma `applyMaybe` mb

lift3 :: (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
lift3 f ma mb mc =
  mapMaybe f ma 'applyMaybe' mb 'applyMaybe' mc
```
- type class called applicative (as opposed to functor)
```Haskell
type Person = String
parent :: Person -> Maybe Person
parent p = undefined

grandparent :: Person -> Maybe Person
grandparent p =
  case (parent p) of
    Nothing -> Nothing
    Just pp -> parent pp
```
- but what if we want more layers of parent?
```Haskell
fancyApplyMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
fancyApplyMaybe f Nothing  = Nothing
fancyApplyMaybe f (Just a) = f a
```
- now we can use this to define the grandparent function
```Haskell
grandparent :: Person -> Maybe Person
grandparent p =
  -- fancyApplyMaybe parent (parent p)
  parent `fancyApplyMaybe` (parent p)
```
- then there's this thing:
```Haskell
fancyApply :: Monad_ t => (a -> t b) -> t a -> t b
```
cya


### 10.15.18 Misc
```
foldr foo init [x1, x2, x3]
foldr foo init (x1:x2:x3:[])
foo x1 (foldr foo init (x2:x3:[]))
foo x1 (foo x2 (foldr foo init (x3:[])))
foo x1 (foo x2 (foo x3 (foldr foo init [])))
foo x1 (foo x2 (foo x3 (init)))
x1 `foo` (x2 `foo` (x3 `foo` (init)))
```
- kinds of types
- ```:kind Bool``` is type * (doesn't take any more arguments)
```Haskell
cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct as bs = [(a, b) | a <- as, b <- bs]
```
- list comprehensions

### 10.12.18 IO
- most languages use void as type... that doesn't give a lot of information
```Haskell
main = putStrLn "Hello, world!"
:t main -> main :: IO ()
```
- IO is a computation that interacts with the user and outputs unit
- getLine is not a function (type IO String)
- do notation/block 
```
do {
  action1; -- IO T1
  action2; -- IO T2
  action3; -- IO T3
} :: IO T3
```
- type is that of the last action
- how to store getLine (can't use let, that would make the variable a synonym for getLine)
``` import System.Environemnt ```
- way to ask for the environment variables (when Haskell was launched)
- sometimes you need to add return to make types match
- use ```read```

###### full code
```Haskell
module Main where


import System.Environment



main :: IO ()
main = do
   putStrLn "Hello, world! How are you?"
   s <- getLine
   putStrLn $ "great you are " ++ s

login :: IO ()
login = do
  putStrLn "What's your username?"
  s <- getLine
  user <- getEnv "USER"
  if s == user
    then putStrLn $ "Thank you " ++ s
    else do
         putStrLn "Filthy LIES"
         login

-- ask :: IO Int
ask = do
  putStrLn "What's your favorite number?"
  s <- getLine
  putStrLn $ "That's a bad number. " ++ show (read s - 1) ++ " is better"
  -- return s
```



### 10.10.18 Type Classes
- what can't be equated?
  - different types, functions, etc
- type classes start with capital letters
``` class Eq_ a where ```
- gonna define what it means for set of types a to be in the class
```Haskell
data List
  = Nil
  | Cons a (List a)
```
- Nil is empty list, Cons has value a (the head) and another List (the tail)
- have to define stuff
comparison defaults:
```Haskell
data List
  = Nil
  | Cons a (List a)
```
- this way you only need to define one, and the other will be automatically defined
instance:
```Haskell
instance Eq_ IntList where
  Nil == Nil = True
  Cons n ns == Nil = False
  Nil == Cons n ns = False
  Cons m ms == Cons n ns = n == m && ns == ms
```
- You could replace two middle cases by adding ```_ == _ = False``` at the end
- constraint on type a
  - if for every type a that is in Eq already, then its the case that list of a is gonna be in eq, based on the defintion in this class. Will only work for types where the value is known to be an Eq
 
```Haskell
class Eq a => Ord_ a where
  (<=) :: a -> a -> Bool
```
- Ord only makes sense if a is already an Eq
- also wtf is going on in this class
  

### 10.08.18 Functions
- ```let add1 x = 1 + x``` is actually syntactic sugar
- ```\x -> 1 + x``` given x, return 1 + x. The backslash represents a lambda
- ```:t \x -> 1 + x``` returns ```\x -> 1 + x :: Num a => a -> a```
- ```\x -> \y -> x + y :: Num a => a -> a -> a```
- multi-argument functions are just a sequence of nested single-argument functions
  - after you give it the first argument, what it computes is another function
- you could do ```\x y -> x + y```
```Haskell 

doubleList :: {- forall n. -} Num n => [n] -> [n]
doubleList ns = map ((*) 2) ns

squareList :: {- forall n. -} Num n => [n] -> [n]
squareList ns = map (\x -> x^2) ns
```
- don't have to define square as long as it's still readable
- ```(^) :: (Num a, Integral b) => a -> b -> a```
- how to reverse?
- ```let reverseCaret b a = a ^ b``` 
- abstracted: ```let reverse f b a = f a b```
```Haskell
reverse :: (a -> b -> c) -> b -> a -> c
reverse f b a = f a b
```
- why not ```a -> a -> a```? bc they don't need to have the same type. Take arbitrary function f and reverse its arguments
  - ex. (^) has different argument types but cant still be reversed
- in the standard library, reverse is called flip
```Haskell
squareList :: {- forall n. -} Num n => [n] -> [n]
squareList ns = map (flip (^) 2) ns
```
- ^^^ another way without defining square
- ```let add3 = (3+) -- {- (+) 3 == \y -> 3 + y -}```
- ```let add3 = (+3) -- {- \x -> x + 3 -}```
- ```let add3 = (+3) -- {- flip (+) 3 == \x -> x + 3 -}```
```Haskell
squareList :: {- forall n. -} Num n => [n] -> [n]
squareList ns = map (^2) ns
```
- ^^^ another way without defining square
```Haskell
squareList = \ns -> let g = (map (^2)) in
                    g ns
squareList = map (^2)
```
- ```f x = g x {- eta-expansion -}```
- ```f   = g   {- eta-reduction -}```
- point-free style (arguments are points, this style is omitting the points)
```Haskell
squareAndDoubleList :: {- for all n. -} Num n => [n] -> [n]
squareAndDoubleList ns = map (\n -> (*2) ((^2) n)) ns
```
- how do we compose functions?
```Haskell
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g = \a -> f (g a)
```
- so using compose:
```Haskell 
squareAndDoubleList :: {- for all n. -} Num n => [n] -> [n]
squareAndDoubleList ns = map (compose (*2) (^2)) ns
```
- compose exists as standard library function under (.)
```Haskell
squareAndDoubleList ns = map ((*2) . (^2)) ns
```
- ```\x -> h (g (f x))```
- what if we don't want to add all these parentheses? use $
- ```($) f x = f x``` <- seems useless at first, but its right associative
- ```h $ g $ f $ x``` <- don't have to use nested parens

- full code:
```Haskell

doubleList :: {- forall n. -} Num n => [n] -> [n]
doubleList ns = map ((*) 2) ns

-- squareList :: {- forall n. -} Num n => [n] -> [n]
-- squareList ns = map (\x -> x^2) ns

-- squareList :: {- forall n. -} Num n => [n] -> [n]
-- squareList ns = map (flip (^) 2) ns

-- squareList :: {- forall n. -} Num n => [n] -> [n]
-- squareList ns = map (^2) ns

squareList = \ns -> let g = (map (^2)) in
                    g ns
squareList = map (^2)

reverse :: (a -> b -> c) -> b -> a -> c
reverse f b a = f a b

squareAndDoubleList :: {- for all n. -} Num n => [n] -> [n]
-- squareAndDoubleList ns = map (compose (*2) (^2)) ns
squareAndDoubleList ns = map ((*2) . (^2)) ns

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g = \a -> f (g a)

```

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
  deriving (Show)
```
- so now when you try ``` map getName [Instructor "Ravi" [], Student  "Christian" "asdafsa" 100000, Parent 10] ```
- you get ``` [Success "Ravi",Success "Christian",Error] ```
- anything else to make it more useful?
  - make it more generic, what if we want getAge? but we defined MaybeError to hold a String in the case of success
  - define it to take type parameter as an argument
```Haskell
data MaybeError a
  = Error -- String -- "error"
  | Success a
  deriving (Show)
```

##### Full code
```Haskell

-- type {- alias -} String = [Char] <- already defined in Prelude

data Person
  = Student String String Int
  | Instructor String [String]
  | Parent Int
  deriving (Show)

--  [Student, Instructor, Instructor] works because they are the same type

getName :: Person -> MaybeError String
getName (Student name _ _) = Success name
-- use data constructors to pattern match values (underscores are like wildcards)
getName (Instructor name _) = Success name
-- getName (Instructor "ravi" []) -> "ravi"
-- map getName [Instructor "Ravi" [], Student  "Christian" "asdafsa" 100000] -> ["Ravi", "Christian"]
-- getName (Parent _) = "No Name"
getName (Parent _) = Error

getAge :: Person -> MaybeError Int
getAge (Parent i)    = Success i
getAge _             = Error
-- don't do it the other way around (underscores take everything so it would also throw error for Parent!)

data MaybeError a
  = Error -- String -- "error"
  | Success a
  deriving (Show)

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
