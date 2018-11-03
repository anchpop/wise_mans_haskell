---
title: Wise Man's Haskell
author: Andre Popovitch
date: !today
---


# Preface

!newthought(I'm Andre Popovitch). This is my book on learning Haskell, an excellent functional programming language. It's not finished yet but hopefully it will be soon - it should be quite useful already. I assume some programming knowledge, but nothing too in depth. If you've played with Python or Javascript and know how to open a command prompt/terminal, that'll be plenty. 

**What to do if you see a typo**: Mention me with @likliklik in the [Functional Programming Discord](https://discord.gg/6XQC7KA), [email me](mailto:andre@popovit.ch), or leave an issue on [the GitHub](https://github.com/anchpop/wise_mans_haskell). These are also great ways to ask questions!

!newthought(I wrote) this book because all the other good Haskell tutorials either cost money or were too verbose. That's surprising, considering GHC (the most popular Haskell compiler) has been around since 1992.

This book is heavily inspired by [Learn You A Haskell](http://learnyouahaskell.com/) by Miran Lipovača, 
[Haskell Programming From First Principles](http://haskellbook.com/) by Christopher Allen Julie Moronuki,
[Programming in Haskell](https://www.amazon.com/Programming-Haskell-Graham-Hutton/dp/0521692695) by Graham Hutton,
[Real World Haskell](http://book.realworldhaskell.org/) by Bryan O'Sullivan, John Goerzen, and Don Stewart,
and [Le Petit Prince](https://www.amazon.com/Petit-Prince-French-Antoine-Saint-Exup%C3%A9ry/dp/1502878267) by Antoine de Saint-Exupéry.

**A note on exercises**: I've put a few exercises in here, and they're very important for reinforcing your knowledge. **Don't skip them**!marginnote(This book also has margin notes scattered throughout, I recommend reading them because they often provide extra context or supplemental info.)

I said this book wouldn't be verbose, so let's move on. And have fun!


# Functional Programming and Purity

This section is for people with previous experience programming. If you have no previous experience, feel free to skip this chapter.

!newthought(Haskell is) strange among many languages you might be used to. It is a pure and functional programming language!marginnote(Haskell is also *lazy*, but we'll touch on that later.). 
If you know what this means, you can skip to the next chapter. Otherwise, read on! 

1) Functionality is a somewhat vague concept. It doesn't mean other languages are dysfunctional!sidenote(Although they are.). Once set, variables cannot be changed!marginnote(When a variable changes, functional programmers sometimes call it a *mutation*. Mutations are not allowed in Haskell.). If you write `a = 2`, you cannot follow it with `a = 3`. 

    This makes your programs simpler. No longer do you have to keep track of 100 different variables, all constantly changing!marginnote(This is called having a lot of *statefulness* in your program, and many programmers try to avoid it even when using other types of programming languages.).Another term for this is that everything in Haskell is *immutable*. There are other benefits too. For example, writing programs that run on multiple cores or the GPU becomes much easier. 

2) Purity means that when writing a function, the function's output should depend only on its input, and all functions should be *side-effect free*, meaning they do nothing except return a result. This also makes your programs simpler - you know that when you call `addOne` with the parameter `2`, the only thing it can do is return a number, so there should be no surprises. 

# Getting started

!newthought(To program) in Haskell, you should be familiar with how to navigate your terminal!marginnote(If you're on Windows, what you call the command prompt is called the "terminal" elsewhere. Also, you should install  [Full Cmder](http://cmder.net/), and use that instead of `cmd`.). To actually use Haskell, you should download and install [The Haskell Platform](https://www.haskell.org/platform/), which has everything you need to get started quickly. It includes GHC!marginnote(**GHC** stands for *Glasgow Haskell Compiler*), the most popular Haskell compiler, along with useful tools such as Stack and Cabal. 

Once you've done that, open a terminal and type `ghci`!marginnote(The *i* in GHCi stands for *interpreter*, which is a simple interface for playing with Haskell). If you already had one open, you might need to close and reopen it. Once you've entered that, you should see something like:

```haskell
GHCi, version 8.4.3: http://www.haskell.org/ghc/  :? for help
Prelude>
```

If you need to get out of GHCi, you can type `:quit`!marginnote(`:q` will work instead of `:quit`, if you feel the need to save a couple keystrokes.).

Where you see `Prelude>` is you can type Haskell expressions or statements, and see what they evaluate to. If you type an expression, GHCi will tell you the result of that expression. Try entering our friend from the last chapter, `1 + 2`. 

```haskell
Prelude> 1 + 2
3
```

If you try that and see `3`, all is well. You can also try `-` for subtraction!marginnote(`-` can also be used to negate a number, such as turning `3` to `-3`.), `*` for multiplication, `/` for division, and `^` for exponentiation, and ``` `rem` ``` for remainder!marginnote(``` `rem` ``` divides two numbers and returns the remainder. It's usually used to check if two numbers are divisible). If you play around with these in GHCi, it might look something like this (To explain what's going on, we've added some *comments*, which begin with two `--` hyphens and are strictly for educational purposes):

```haskell
-- This is what comments look like.
-- You can type a comment into GHCi, but it won't do anything.
Prelude> (13 * 4) / 3
17.333333333333332
Prelude> 3 ^ 3 ^ 3 ^ 2
-- Omitted, but it's a pretty big number.

-- Let's check if some numbers are divisible by 3 using `rem`, 
-- which tells us the remainder of the left argument divided by the right.
Prelude> 10 `rem` 3 == 0
False
Prelude> 12 `rem` 3 == 0
True
```

You can define constants, just as you might expect. 

```haskell
Prelude> i = 3
Prelude> 4 * i
12
```

There's few restrictions that to what you can name a constant, other than that it cannot begin with an uppercase letter.

These operators we keep using, like `+` and `*`, are all examples of things called functions - let's try writing some of our own! In GHCi, type

```haskell
Prelude> addOne x = x + 1
Prelude> addOne 3
4
```

Can you see what's going on here? We've written a function called `addOne`!marginnote(It's convention in Haskell that if a function name is composed of multiple words, the first letter of each word after the first one is capitalized. That's why we have `addOne`, not `addone`. In fact, Haskell won't let you start a function name with an uppercase letter.), which does exactly what it says on the tin (If you're coming from other languages, notice that Haskell doesn't use parentheses or commas in function calls). When we write `addOne 3` it's the same as writing `3 + 1`. You can think of `addOne` as a kind of machine - you put a number in, and it spits a different number out. In our case, the number it spits out is always one higher than the one you put in. We say `addOne` *takes* a number as a parameter!marginnote(While they're not technically equivalent, we will use the words "parameter" and "argument" interchangeably.), and *returns* a different number.

!figure(Demonstration of how `addOne` works.)(/static/wise_mans_haskell/assets/exp_addOneDemo.svg)

---

__***Exercises***__:

1) Write a function that adds two to a number, called `addTwo`. 

---

!newthought(You can) have a function with more than one variable, too!marginnote(As we'll discuss later, functions with more than one variable are somewhat of an illusion in Haskell.). Let's see how that works.

```haskell
Prelude> addThenDouble x y = (x + y) * 2
Prelude> addThenDouble 2 4
12
```

Let's break down what's going on here.

```haskell
  addThenDouble   x y    =   (x + y) * 2
--    [1]         [2]   [3]     [4]
```

1) `addThenDouble` - The name of the function we're defining.

2) `x y` - The names of the *parameters* of our function. By having two, *x* and *y*, we're saying this function takes two parameters. Your parameters can be named whatever you want, as long as it doesn't begin with an uppercase letter and isn't already taken by the name of a function. 

3) `=` - "equals". You can read this as a separator between the left half and the right half. The `addThenDouble x y` is the function's name and parameters - the right `(x + y) * 2` is what the function does.

4) `(x + y) * 2` - What the function does to the two numbers that you gave it. In our case, it adds them and then multiplies the result by two.

This is a function that takes two numbers, adds them together, then doubles the result. Writing `addThenDouble 2 4` is the same as writing `(2 + 4) * 2` or just `12`. 

You're able to use your custom functions when writing new ones, and it's where the real power of functions appears:

```haskell
Prelude> addOneTwice x = addOne (addOne x)
Prelude> addOneTwice 4
6
```

The parentheses are used to tell Haskell we want to do `addOne x` first, then run `addOne` on the result of that. Omitting these parentheses will result in an error. So we've seen that we can have functions with one or two variables, but how about... zero variables?

```haskell
Prelude> always3 = 3
Prelude> always3
3
```

If you like, you can think of this as a function that takes 0 values and returns `3`!sidenote(The proper name for a function that takes no values would be a *nullary function*.). However, this is better thought of as just a plain value or constant. GHCi will allow you to define `i = 1` and then later define `i = 2`, but this is not allowed in "real" Haskell (which we'll go over in a moment).

!newthought(Now, this) is all well and good, but not very conducive to writing actual programs. For that, you need to be able to write your code into a text file and run it. To do that, make a new file somewhere on your computer called `myProgram.hs`!marginnote(`.hs` is the extension used for most Haskell programs.). Inside it, write:

```haskell
-- myProgram.hs
!include(haskelltests/should_compile/myProgram.hs)
```

The comments!sidenote(starting with `--`, remember) at the top of these examples are what we named the file in our own tests. Now, in your terminal, navigate to the directory where the file is located!marginnote(remember, you can get out of GHCi by typing `:quit`). Then open up GHCi again and type:

```haskell
Prelude> :load myProgram.hs
*Main> addOne 100
```

`:load` is a special GHCi command!marginnote(Anything that starts with a `:` is a GHCi command, not real Haskell.) - it tells GHCi to interpret the file you give it and load it into your interpreter so you can play with it. You defined `addOne` in `myProgram.hs`, so by typing `:load myProgram.hs` you load all the functions in that file.

You might have also noticed `Prelude>` changed to `*Main>`, and you'll see why that happens in the future. But for now, you can have fun playing with it - try running `:set prompt "Haskell is fun> "`.

---

__***Exercises***__:

1) What do you think that the following function will do?

    ```haskell
    foo x = x * 3
    ```

    If you're unsure, you can try entering it into GHCi.

2) In GHCi, write a function called `circumference`, which takes a number and multiplies it by `3.14`.

3) In GHCi, write a function called `doubleDifference` that takes two values, subtracts the second from the first, then doubles the result. Stuck? Try looking at some of the functions we made earlier in this chapter.

---

# Types and type signatures

## More types!

!newthought(You've already) seen you can set a constant to a number:

```haskell
Prelude> i = 1
Prelude> i
1
```

But there's lots of other *types* of things you can set constants on, too! Go back to GHCi and try this:

```haskell
Prelude> c1 = 'a'
Prelude> c2 = 'b'
Prelude> c1
'a'
Prelude> c2
'b'
```

What you're seeing here is we can set a constant to a `Char` (a single character), instead of a number. Do `String`s work?

```
Prelude> s1 = "abc"
Prelude> s1
"abc"
```

They do! In Haskell, you use single quotes for `Char`s and double quotes for `String`s. 

Booleans are also available in Haskell, like so:

```haskell
Prelude> a = True
Prelude> a
True
Prelude> b = False
Prelude> b
False
```

There are (almost) infinite different numbers you can choose from, but Booleans only have two allowed values: `True` and `False`!sidenote(There's also `undefined`, which is a bit of an exception to every rule and not something you should worry about.). Booleans are called `Bool`s in Haskell. 

!newthought(You can) test the equality of two values using `==`. To see if they're not equal, you use `/=`!marginnote(The `==` and `/=` functions take two values of the same type and return a `Bool`.).

```haskell
Prelude> a = 1
Prelude> b = 1
Prelude> c = 3
Prelude> a == b
True
Prelude> a == c
False
Prelude> a /= c
True
```

That works on numbers, `String`s and `Char`s, too:

```haskell
Prelude> a = 'a'
Prelude> b = 'b'
Prelude> a == b
False
Prelude> a /= b
True
```

There are some functions that take `Bool`s, of course. There's `&&`, which means "and", and `||`, which means "or". These functions each take two `Bool`s and return a new `Bool`.

```haskell
-- You can use && to mean "and" and || to mean "or", too.
Prelude> 1 == 1 && 2 == 2 
True
Prelude> 1 == 1 && 2 == 3
False
Prelude> 1 == 1 || 2 == 2
True
Prelude> 1 == 1 || 2 == 3
True
Prelude> 1 == 3 || 2 == 3
False
```

!newthought(Haskell keeps) track of what *type* everything is!marginnote(You can think of Haskell types as little labels that are attached to everything in your program.). If `i` is a number and `c` is a `Char`, it doesn't make sense to ask if one is equal to another - they're two different types of things, of course they aren't! If you try to run `i == c`, Haskell will make an error, because `==` requires that both of its arguments be the same type. Similarly, multiplication only works on numbers, so Haskell won't let you write `5 * 'a'`. Haskell keeping track of the types of everything is very helpful, and is part of Haskell's secret to prevent you from writing buggy software. This is called a *type system*, and lots of languages have one, but Haskell's is better than most other languages'.

!newthought(Lists are) also possible in Haskell, let's check those out.

```haskell
Prelude> myNumberList = [1, 2, 3]
Prelude> myNumberList
[1,2,3]
Prelude> myCharList = ['a', 'b', 'c']
```

You might have noticed that if you type `myCharList` to tell GCHi to show it to you, then you see:

```haskell
Prelude> myCharList
"abc"
```

What gives? It's saying `"abc"`, but we typed `['a', 'b', 'c']`! Well that's because in Haskell, a `String` is just a List of `Char`s. So when you type `"abc"`, Haskell treats it as of you typed `['a', 'b', 'c']`. And when you ask GHCi to display a List of `Char`s, it shows it as `"abc"` instead of `['a', 'b', 'c']` because `"abc"` is easier to read and write. 

!newthought(Now, you're) going to see a very important GHCi command. It's called `:type`, or `:t` for short. Almost everything in Haskell has a type, and `:t` tells you what that type is. Let's try on some of our constants:

```haskell
Prelude> a = 'a'
Prelude> :type a
a :: Char
```

This is GHCi telling us our 'a' is of type `Char`. The `::` is just a bit of syntax GHCi uses to tell you what it's talking about, it doesn't affect the type. 

```haskell
Prelude> s1 = ['a', 'b', 'c']
Prelude> s2 = "abc"
Prelude> s1 == s2
True
```

You can have lists of other things too, by the way.

```haskell
Prelude> i1 = [1,2,3]
Prelude> i1
[1,2,3]
Prelude> i2 = ["hello", "world"]
Prelude> i2
["hello","world"]
```

`i1` is a List of numbers. `i2` is a List of `String`s. And since a `String` is really just a List of `Char`s, `i2` is a List of Lists of `Char`s!marginnote(Note one rule about Haskell Lists: everything in them has to be of the same type. If you try `i = [1, 2, "a"]`, GHCi will give you an error.)!

Lists have some useful features. You can use `++` to concatenate two lists.

```haskell
Prelude> s1 = "Hello"
Prelude> s2 = "World"
Prelude> s1 ++ s2
"HelloWorld"
```

Whoops! We forgot to put a space in there.

```haskell
Prelude> s1 = "Hello"
Prelude> s2 = "World"
Prelude> s1 ++ " " ++ s2
"Hello World"
```

Let's write a function to do this for us, so we don't have to it every time.

```haskell
Prelude> concatWithSpace l1 l2 = l1 ++ " " ++ l2
Prelude> concatWithSpace "Hello" "World"
"Hello World"
```

See, it's pretty easy, huh? If you'd like to be able to put `concatWithSpace` between the two `String`s, like you can with `++`, simply surround it with  ``` ` ``` backticks.

```haskell
Prelude> "Hello" `concatWithSpace` "World"
"Hello World"
```

If all you need to do is put a value at the beginning of a List, you can do so with the `:` operator!marginnote(You might want to remember this, because it turns out to be a very fundamental operation for Lists!).

```haskell
Prelude> 1:[2,3]
[1,2,3]
```

## Type signatures

!newthought(As you) just learned, when you have a constant, it can be a number (like `i = 2`), a `Char` (like `c = 'x'`), or a `String` (like `s = "hello"`), a List of Strings, a List of Numbers, or one of many other types.

!marginfigure(Some examples of types and type signatures.)(/static/wise_mans_haskell/assets/exp_typesAndSignatures.svg)

In Haskell, every value has a type. We can figure out a value's type with the `:type` command in GHCi, which shows us it's *type signature*.  A type signature is just a way of writing down a value's type. You can also use `:t` instead of `:type`. Let's try it out.

```
Prelude> i = 'a'
Prelude> :type i
i :: Char
```

GHCi is trying to tell you that `i` has the type `Char`. To do that, it says `i :: Char`. You can read `::` as "has the type". Let's try another.

```haskell
Prelude> s = "hello!"
Prelude> :type s
s :: [Char]
```

This is GHCi telling you `s` has the type `[Char]`, which means it's a List of `Chars` (A `String` is the same as `[Char]`). The square `[]` brackets are telling us it's a List of Chars, not just a regular `Char`.). Do you remember what type `True` is?

```haskell
Prelude> :type True
True :: Bool
```

Ah yes, a `Bool`. But even functions have types! Let's write a function to test this, `addABang`, which takes a `String` and puts an exclamation mark at the end.

```haskell
Prelude> addABang s = s ++ "!"
Prelude> addABang "hello"
"hello!"
```

Works like a charm. Now let's see what type it has.

```haskell
Prelude> :type addABang
addABang :: [Char] -> [Char]
```

The `->` arrow means we're dealing with a function. In our case, a function that takes a `[Char]` and returns a `[Char]`.

Remember the `concatWithSpace` function? We can use `:type` to see what type it has too.

```haskell
Prelude> concatWithSpace l1 l2 = l1 ++ " " ++ l2
Prelude> :type concatWithSpace
concatWithSpace :: [Char] -> [Char] -> [Char]
```

You might be wondering what multiple `->` arrows mean. That's how functions with multiple parameters are typed in Haskell. The thing after the last arrow is the output, all the rest are the parameters. So `concatWithSpace` takes two `[Char]`s and returns a `[Char]`!sidenote(You might be wondering why it's done like this. Isn't it confusing that the return type looks just like a parameter? Well, it has to be this way, because of a Haskell feature called *currying* (which will be explained in a later chapter).).

```haskell
  concatWithSpace :: [Char] -> [Char] -> [Char]
--                   ================    ======
--                        input          output
```

When writing Haskell functions, it's usually a good idea to write the type signatures explicitly, even though you usually don't need to. Let's make a new file, `testingTypes.hs`. Inside it, put:

```haskell
-- testingTypes.hs
!include(haskelltests/should_compile/testingTypes.hs)
```

What we did here is write a type signature for our function. This just means we're telling Haskell, and also anyone who reads this code, that `concatWithSpace` should have the type `[Char] -> [Char] -> [Char]` (meaning it takes a `[Char]`, then another `[Char]`, then returns a `[Char]`). This is important so let's go over that once again. Here is our type signature:

```haskell
    concatWithSpace   ::    [Char]  ->  [Char]  ->  [Char]
--  ===============   ==    ==============================
---        [1]        [2]                [5]     
```

1) `concatWithSpace`: This tells Haskell that this type signature is for the `concatWithSpace` function.

2) `::`: This separates the name of the function with the type we're saying it has. You can read this as "is a".

3) `[Char]  ->  [Char]  ->  [Char]`: This is the type of the function. This function takes a `[Char]`, then another `[Char]`, then returns a `[Char]`.

`++`, `==`, `+`, `-`, `/`, `*`, and `^` are all functions. Because they're written only with special characters, they're also called *operators*. Operators are by default *in*fix functions, which mean they go *in* between their parameters, e.g. `1 + 2`. 

You can turn an infix function into something that works more like a regular function by surrounding it in parentheses. So instead of writing `1 + 2`, you can also write `(+) 1 2`. Do you remember the `++` operator? It works on lists of any type, which you might think is strange. What could its type possibly be?  

```haskell
Prelude> "abc" ++ "123"
"abc123"
Prelude> [True, True] ++ [False, False]
[True,True,False,False]
```

See? It works on `[Char]` just as well as it works on `[Bool]`. Luckily, it's easy for us to find out its type, just use `:type`! But `:type` wants a regular function, not an infix function, so you have to surround `++` in parentheses.

```haskell
Prelude> :type (++)
(++) :: [a] -> [a] -> [a]
```

Strange! Instead of writing `[Char]` or any other real type, they've written `[a]`. Notice the lowercase letter - all real types start with uppercase letters. This means `a` is just a placeholder, or a *type variable*, for any type you want to give the function. When you pass `++` a `[Char]`, Haskell sees that `[Char]` can fit into `[a]` (since `a` can be any List). But it also requires consistency! Once Haskell knows that `a` is `Char` for this function call, then you can't pass it an `[Int]` or `[String]` or anything besides `[Char]`. This means that if the first parameter is `[Char]`, the rest have to be, as well.

So this is ok:

```haskell
Prelude> "Hello, " ++ "world!"
```

Because the two parameters are both `[Char]`s, `a` can equal `Char` just fine. But this will give you an error:

```haskell
Prelude> [1, 2, 3] ++ [True, True, False]
```

That's because once you've given `++` a `[1, 2, 3]`, that means `a` has to be a number. Then when you give it `[True, True, False]`, Haskell knows `a` has to be a number, not a `Bool`, so it doesn't work. 

Here's another example. The `head` function takes a List and gives you the first element of it. 

```haskell
Prelude> head [1, 2, 3]
1
```

What do you think its type will be?

```haskell
Prelude> :type head
head :: [a] -> a
```

So this is telling us `head` takes a List of some type, and returns an element of that type. This is what we would expect! It'd be very strange if we gave it a List of `Bool`s and it said the first element was a `Char`.

---

__***Exercises***__:

1) What type will `head` give us if we give it a `[Char]`?

2) There's a function called `tail` which is similar to `head`. It takes a List and returns a List of everything except the first element. So `tail "abc" == "bc"` and `tail [1,2,3] == [2,3]`. Can you guess what type signature tail has? To check your answer, run `:type tail` in GHCi.

---

## Typeclasses

!newthought(In most) programming languages, but especially Haskell, there are some types that aren't exactly the same but have some things in common. For instance, take the following Haskell types:

1) `Integer` - Exactly what it sounds like. Integers are numbers like `-1`, `0`, `1`, or `1000`, but not `2.5`. It has no maximum or minimum value, but if you try to express too large of a number, eventually your computer will run out of memory.

2) `Int` - This is the same "int" you might be familiar with from other languages. It has a maximum and minimum value, which can vary depending on what machine you're on, but it is usually very large (generally the maximum is 2^31-1 and the minimum is -2^31). The tradeoff is that it's more efficient than `Integer`.

3) `Float` - Short for floating point. This holds numbers that can have decimal points, like `1.4` or `-2.5`, but can still hold whole numbers, like `2`. The drawback is that sometimes the numbers it holds aren't exactly the number you think it is. This causes the scary [*floating point precision error*](https://stackoverflow.com/questions/2100490/floating-point-inaccuracy-examples), where answers end up being just a little bit away from what they should be. Enter `0.1 + 0.2` into GHCi to see how easy it is to trip up on this.

4) `Double` - This is the same as a `Float`, but it's twice as precise. You can still run into floating point precision errors though.

All four of these seem kind of similar. If you write a function that multiplies a number by two, it'd be nice you didn't have to say specifically what type it takes, and could instead just say you want it to take in a number. Well luckily, that is very easy!sidenote(Uniquely easy, in fact. In languages like C++ you'd have to do all kinds of fiddling with templates to get a result that still isn't as nice as Haskell's.)! Let's write a function that doubles `Int`s.

```haskell
intDoubler :: Int -> Int
intDoubler x = x * 2  
```

!newthought(All of) the types mentioned above are inside a *typeclass*!marginnote(Typeclasses have nothing to do with classes from most other languages! Don't get confused.) called `Num`. That means you can use `+`, `-`, `*`, and `abs` on them (`abs` returns the absolute value of a number). Now, we'll show you how to modify `intDoubler :: Int -> Int` with some new syntax to allow it to work on anything in the `Num` typeclass, not just `Int`:

```haskell
numberDoubler :: (Num a) => a -> a
numberDoubler x = x * 2  
```

Let's take a closer look at that type signature

```haskell
     numberDoubler  ::   Num a =>    a  ->  a
--   ============   ==   ========    ========   
--        [1]       [2]     [3]         [4]
```

1) `numberDoubler` - The name of our function. 

2) `::` - Remember, you can read this as "is a".

3) `Num a =>` - Puts a restriction on the type variable `a`. Just `a` on its own can be anything, but when you put `Num a =>` at the beginning of a type signature, whatever type `a` takes the place of is constrained to be part of the `Num` typeclass. This is called a *type constraint*, because it constrains what types are allowed (`a` would normally allow any type, but because of the type constraint `a` now has to be a number). You can think of `=>` as using whatever is on the left side to constrain the right side.

4) `a  ->  a` - The part of the type signature that should be familiar to you. It means we're dealing with a function that takes an `a` and returns an `a`. Thanks to `Num a =>`, we have the constraint that `a` be inside the typeclass `Num`, but otherwise, we aren't picky.

The result is a function that can double anything in the `Num` typeclass, which is all our number types!sidenote(If this seems intimidating, read on, more examples will make it clear.)!

## Handy typeclasses and more examples

!newthought(You'll see) here that a type can be a part of more than one typeclass. These are some typeclasses you'll be seeing a lot of:

1) `Eq` - This is the typeclass for anything that can be tested for equality, using the `==` and `/=` functions. Most types, including `Char`s, `String`s, and everything in the `Num` typeclass, fall under `Eq`!sidenote(In fact, almost all types except functions are inside the `Eq` typeclass.).

    ```haskell
    Prelude> 2 == 2
    True
    Prelude> :type (==)
    (==) :: Eq a => a -> a -> Bool
    ```

    You can see `==` is a function that takes two values of type `a`, where `a` is inside the `Eq` typeclass, then returns a `Bool`. 

2) `Ord` - This is the typeclass for things that can be put in an order. They have functions like `>`, `>=`, etc. Anything that is part of the `Ord` typeclass is automatically part of the `Eq` typeclass!sidenote(`Ord` is separate from `Num` because, for example, letters can be put in alphabetical order. Also, some numbers can't be ordered, like [complex numbers](http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Complex.html).).

    ```haskell
    Prelude> 3 > 5
    False
    Prelude> :type (>)
    (>) :: Ord a => a -> a -> Bool
    ```

    `>` takes two values of type `a`, where `a` is of the typeclass `Ord`, and returns a `Bool`.

3) `Show` - This is the typeclass for things that can be converted to strings with the function `show`. That's actually what GHCi does whenever you enter an expression - if it's of typeclass `Show`, then it calls `show` and prints the result!sidenote(If it isn't in the typeclass `Show`, you'll see an error in GHCi.).

    ```haskell
    Prelude> show 2
    "2"
    Prelude> :type show
    show :: Show a => a -> String
    ```

    `show` takes a value of type `a`, where `a` is of the typeclass `Show`, and returns a String.


4) `Read` - This is the typeclass for things that can be converted from strings with the function `read`. It's like the inverse of the Show typeclass. It's a bit tricky because if it doesn't know what type to return it'll give you an error. Luckily we can tell it with `::`!sidenote(Which should be familiar to you, as it's the same `::` you see in type signatures!):

    ```haskell
    Prelude> read "5"
    -- error! read doesn't know if we want an Int, an Integer, etc.
    Prelude> read "5" :: Int
    5
    ```
    
    Let's look at the type of `read`, as well:

    ```haskell
    Prelude> :type read
    read :: Read a => String -> a
    ```

    So `read` takes a string and returns a value of type `a`, where `a` is part of the `Read` typeclass.

5) `Integral` and `Floating`. - `Integral` contains the types `Int` and `Integer`, and `Floating` contains the types `Float` and `Double`. You'll see them often in one useful function, `fromIntegral`, which can convert an `Integral` into whatever `Num` is necessary:

    ```haskell
    -- Remember, we can force a number to be an Int with, for example, `4 :: Int`.
    Prelude> i = (4 :: Int)
    Prelude> i
    4
    Prelude> f = (3.5 :: Float)
    Prelude> f
    3.5

    -- You can't add values of two different types! 
    Prelude> i + f
    -- error!
    -- But we can convert i, which is an Int, to any other number using fromIntegral:
    Prelude> (fromIntegral i) + f
    7.5
    ```
    
    The `fromIntegral` function is useful when a function returns an `Int` or `Integer` and you need it to be some other number type. Lets see what type it has:

    ```haskell
    Prelude> :type fromIntegral
    fromIntegral :: (Integral a, Num b) => a -> b
    ```

    That's right, this one involves two different typeclasses. If you have more than one typeclass in your function's type signature, they should have `()` parentheses around them and be separated by commas. You can describe this function as taking an Integral and returning a Num.

---

__***Exercises***__:

1) What do you think the type signature of `+` is? Type `:type (+)` to check your answer. 

---

# Lists and Tuples

## Useful Functions on Lists 

!newthought(Lists)!marginnote(Haskell Lists are actually represented internally as singly-linked lists, which has all the performance implications you'd expect. Haskell also has [Arrays](https://www.haskell.org/onlinereport/array.html) which you can use where appropriate. We'll discuss this more in the chapter on writing optimized Haskell.) are very useful in Haskell, and there are a lot of functions that work on lists that you should know about.

1) `x:xs` inserts a value `x` at the beginning of a List `xs`.

    ```haskell
    Prelude> 3:[4,5]
    [3,4,5]
    Prelude> 1:[]
    [1]
    Prelude> 1:2:[4,5]
    [1,2,4,5]
    ```

    This is very important because it's actually how lists are implemented internally. When you type `[10, 20, 30]`, Haskell actually sees that as `10:20:30:[]`.



2) `take x xs` gives you the first `x` values from the List `xs`.

    ```haskell
    Prelude> take 5 [1,2,3,4,5,6]
    [1,2,3,4,5]
    ```

2) `drop x xs` drops the first `x` values from the List `xs`.

    ```haskell
    Prelude> drop 3 [1,2,3,4,5,6]
    [4,5,6]
    ```

3) `head xs` gives you the first value of the List `xs`.

    ```haskell
    Prelude> head [1,2,3,4,5,6]
    1
    ```

4) `last xs`  gives you the last value of the List `xs`

    ```haskell
    Prelude> last [1,2,3,4,5,6]
    6
    ```

4) `init xs` gives you all but the last value of the List `xs`.

    ```haskell
    Prelude> init [1,2,3,4,5,6]
    [1,2,3,4,5]
    ```

4) `tail xs`  gives you all but the first value of the List `xs`.

    ```haskell
    Prelude> tail [1,2,3,4,5,6]
    [2,3,4,5,6]
    ```
5) `xs !! x` returns the `x`th item of the List `xs`. You may also see ``` `elem` ``` which is equivalent. 

    ```haskell
    Prelude> xs = "Ice, ice, baby"
    Prelude> xs !! 0
    'I'
    Prelude> xs !! 11
    'a'
    ```

6) `length xs` returns the length of the List `xs`.

    ```haskell
    Prelude> length "Ice, ice, baby"
    14
    ```

7) `null xs` returns whether the List is empty. It is better to use this than `xs == []`, but it does the same thing.

    ```haskell
    Prelude> null "Ice, ice, baby"
    False
    Prelude> null []  
    True  
    ```

8) `sum xs` takes a List of numbers `xs` and returns their sum.

    ```haskell
    Prelude> sum [1,2,5]
    8
    ```

8) `product xs` takes a List of numbers `xs` and returns their product.

    ```haskell
    Prelude> product [1,2,5]
    10
    ```
9) `maximum xs` and `minimum xs` do what you would expect, they take a List of values in the typeclass `Ord` and find the maximum or minimum one, respectively. 

## Enumerations and Infinite Lists

!newthought(Haskell provides) some easy ways to get a range of numbers, or other things that can be enumerated, using the `..` syntax.

```haskell
-- Get the numbers between 1 and 10 inclusive.
Prelude> [1..10]
[1,2,3,4,5,6,7,8,9,10]

-- Get the letters between 'a' and 'r' inclusive.
Prelude> ['a'..'r']
"abcdefghijklmnopqr"

-- Get the numbers between 1 and 10, with a step of 2.
Prelude> [1,3..10]
[1,3,5,7,9]

-- The step is mandatory if you want to go downwards.
Prelude> [10,9..1]
[10,9,8,7,6,5,4,3,2,1]
```

You don't have to provide an upper limit to your List, either!

```haskell
-- This makes a List containing [1,2,3,4,5,...] all the way up to infinity.
Prelude> [1..]
-- If you type this in GHCi, it will keep showing numbers until you hit ctrl+c, which is how you tell a Haskell command to stop running.

-- This makes a List containing [0,10,20,30,...] all the way up to infinity.
Prelude> [0,10..]
-- Again, have your control-c key ready.

-- To do anything useful with infinite lists, you usually need to limit which ones you get.
Prelude> take 10 [10,20..]
[10,20,30,40,50,60,70,80,90,100]
```

!newthought(Infinite lists) work because Haskell is *lazy* - it won't compute anything until it absolutely has to!sidenote(which is typically when it needs to be displayed on the screen, written to a file, or something similar.). If you do something that forces Haskell to evaluate the whole list, it will never finish, as the list is infinite. Be sure to do something like `take 10` on the list before you try and print it, so you don't get stuck doing stuff forever. 

## List Comprehensions

!newthought(If you've) used List comprehensions in Python, these should be pretty familiar. They're ways to turn one List into a new List - by filtering it and transforming the values.

```Haskell
Prelude> xs = [1..5]
Prelude> xs
[1,2,3,4,5]

-- Here is a List comprehension that takes all the values in xs and does nothing with them.
Prelude> [x | x <- xs]
[1,2,3,4,5]

-- Here is a List comprehension that doubles all the values in xs.
Prelude> [x * 2 | x <- xs]
[2,4,6,8,10]

-- Here is a List comprehension that squares all values in xs, and keeps the result if the value is higher than 10.
Prelude> [x ^ 2 | x <- xs, (x ^ 2) > 10]
[16,25]
```

Let me break down the structure of a List comprehension:

```haskell
    [x ^ 2 | x <- xs, (x ^ 2) > 10]
--   =====   =======  ===========
--    [1]      [2]        [3]
```

1) `x ^ 2`: This is the value that will appear in the final List. `x` is a variable that comes `#2`. This part is used to transform the contents of the List.

2) `x <- xs`: This can be translated as "iterate over the values in the List `xs` and assign them to `x`". It's used to decide which List you want to draw from.

3) `x * x > 10`: This is a condition!marginnote(You can also have multiple conditions here, separated by commas.) that determines whether the result of `#1` will be included in the List. This is used to filter the contents of a List.

In Python, this would be written as `[x*x for x in xs if (x * x) > 10]`.

## Tuples

!newthought(let's say) you wanted to have a List of points, where a point is an `x` and a `y` coordinate. You could have a List of Lists, like this:

```haskell
Prelude> points = [[(0 :: Double), 1], [-2, 4], [5, 2]]
Prelude> :type points
points :: [[Double]]
```

But there's an issue here. If you accidentally forgot a coordinate in your point, that's certainly an error, and Haskell wouldn't warn you!marginnote(An important philosophy in Haskell is that it should be as hard as possible for you to make an error that the compiler doesn't warn you about. We do that by structuring our program so incorrect states (such as points with only one coordinate) are forbidden by the type system. The more you do this, the more bug-free your code will be.)! 

So a better solution is to use tuples. You make a tuple the same way you make a List, but you use `()` parentheses instead of `[]` square brackets.

```haskell
Prelude> points = [((0 :: Double), (1 :: Double)), (-2, 4), (5, 2)]
Prelude> :type points
points :: [(Double, Double)]
```

You'll notice the type signature changed! Tuples are different from lists. They don't have to be homogenous (you can have multiple different types in the same tuple), so their type is the type of each of their elements. A tuple containing two doubles will have the type `(Double, Double)`.!marginnote(The order is important. If the tuple's type is `(String, Bool)` you can't assign `(True, "Hello")` to it.)

!newthought(A tuple) with 2 items is called a 2-tuple, a tuple with 3 items is called a 3-tuple, etc. 2-tuples and 3-tuples are also called doubles and triples, respectively.!marginnote(You can have a tuple with 1 or 0 items, but there wouldn't be much point.)

!newthought(If you) have a 2-tuple, you can extract values from it using the `fst` and `snd` functions!marginnote(Their type signatures are `fst :: (a, b) -> a` and `snd :: (a, b) -> b` respectively.). For example, `fst (1, 2) == 1` and `snd (1, 2) == 2`. Tuples are useful when you want to have a function that returns multiple values. For example, you could imagine a person's name being represented as a 2-tuple, where the first element is their given name and the second item is their family name. Here's a function that will tell you the full name of a child, given their first name and their parent's full name!sidenote(Of course, don't use this in production, as not everyone has a name that fits this schema.).

```haskell
Prelude> myName = ("Jane", "Doe")
Prelude> child'sName parentsName child'sFirstName = (child'sFirstName, snd parentsName)
Prelude> child'sName myName "Claire"
("Claire","Doe")
```

!newthought(Another useful) function is `zip`. It's very simple, so you might even be able to guess what it does just from it's function signature!

```haskell
zip :: [a] -> [b] -> [(a, b)]
```

What it does is take two lists, and *zip* them up into a list of 2-tuples.

```haskell
Prelude> zip [1,2,3] "abc"
[(1,'a'),(2,'b'),(3,'c')]
```

If one list is longer than the other, it cuts off at the shorter list. This is useful because it means you can easily use infinite lists with it!

```haskell
Prelude> zip [1..] "abc"
[(1,'a'),(2,'b'),(3,'c')]
```

---

__***Exercises***__:

1) Above, you say:

    ```haskell
    points = [((0 :: Double), (1 :: Double)), (-2, 4), (5, 2)]
    ```
    
    And the type signature of `points` was `points :: [(Double, Double)]`. What would it have been if we just wrote `points = [(0, 1), (-2, 4), (5, 2)]` instead? Check your answer with `:type points`.
 
---

# More About Functions - Useful Syntax

## Pattern matching

!newthought(You'll notice) we haven't learned how to do if/then/else in Haskell. There is a way, but when possible, it's preferred that you use *pattern matching*!marginnote(Pattern matching (and `case`, discussed below), were created to make it easier to write programs that work well with the type system. If/then/else is useful occasionally, but really struggles with certain Haskell features such as `Maybe`, which we'll discuss later. Also, pattern matching looks nicer.). Let's steal an example from the excellent [Learn You A Haskell](http://learnyouahaskell.com/syntax-in-functions):

```haskell
-- luckyNumber7.hs
!include(haskelltests/should_compile/luckyNumber7.hs)
```

You'll notice that, if you type a number other than 7, we don't really care what it is. We just tell you that you're unlucky. If you don't care about a certain value, it's a Haskell tradition to assign it to the variable `_`!sidenote(Specifically, GHC won't warn you about unused variables if the unused variable begins with a `_`).

```haskell
-- luckyNumber7u.hs
!include(haskelltests/should_compile/luckyNumber7u.hs)
```

This makes implementing recursive functions (functions which call themselves) very easy. Factorial is the usual example:

```haskell
-- factorial.hs
!include(haskelltests/should_compile/factorial.hs)
```

Can you see what's going on here?

0) If you call `factorial 0`, it just returns `1`. 

1) If you call `factorial 1`, it returns `1 * factorial (1 - 1)` which evaluates to `1 * factorial 0` which evaluates to `1 * 1` which evaluates to 1. 

2) If you call `factorial 2`, it returns `2 * factorial (2 - 1)` which evaluates to `2 * factorial 1`. We know `factorial 1` evaluates to `1`, so `2 * factorial 1` evaluates to `2 * 1` which evaluates to `3`.

If you have a pattern that would match any possible value, it is said your pattern is *exhaustive*. Otherwise, it's *non-exhaustive*. Here is an exhaustive pattern:

```haskell
-- isTrue.hs
!include(haskelltests/should_compile/isTrue.hs)
```

`Bool` can only be `True` or `False`, so all the bases are covered. However, this would be *non-exhaustive*:


```haskell
-- writtenNum.hs
!include(haskelltests/should_compile/writtenNum.hs)
```

If you pass `4` to this function, your program will crash!marginnote(Later in this book I will discuss a setting in GHC you can turn on that will force all your patterns to be exhaustive.). A common trick to make patterns exhaustive is to put a catch-all at the bottom:

```haskell
writtenNum :: Integral a => a -> String
writtenNum 1 = "One"
writtenNum 2 = "Two"
writtenNum 3 = "Three"
writtenNum _ = "unknown :("
```

But beware that you don't accidentally put the catch-all at the top! Patterns are checked top-to-bottom, so if you had a catch-all at the top, none of the other patterns could ever be hit.

You can use pattern matching in more interesting ways as well, because you can actually match the *inside* of certain values, such as tuples. Remember the `fst` and `snd` functions from the last chapter? They only apply to 2-tuples. Let's try making some that work with 3-tuples.

```haskell
-- tuplePatterns.hs
!include(haskelltests/should_compile/tuplePatterns.hs)
```

It's kind of a pain having to write these by hand. A later chapter will describe how to write them for all tuples, automatically.

The same idea works with lists. Let's make a function that's like `head`, but gives you the second value instead of the first.

```haskell
-- neck.hs
!include(haskelltests/should_compile/neck.hs)
```

If you're wondering what these weird `:` colons are doing, remember from the chapter on lists. The List you type as `[3]`, Haskell sees as `3:[]`!marginnote(`[3]` gets converted to `3:[]` before any real work is done.). The `String` (really, `[Char]`) you type as `"abc"`, Haskell sees as `'a':'b':'c':[]`. Try entering `'a':'b':'c':[]` into GHCi to prove it to yourself. 

So `a:b:c` can match `1:2:[]` with `a = 1`, `b = 2`, `c = []`. It can also match `1:2:3:4:5:[]` with `a = 1`, `b = 2`, `c = [3,4,5]`!marginnote(Note that, when pattern matching lists with `:`, the first items are single elements and the last item is a (sometimes empty) List.). It will actually match any List with at least two items! This means this pattern is non-exhaustive, since it won't work for any lists with less than two elements, but that's okay for our purposes.

Lets use this newfound knowledge to write a `length'`!sidenote(The `'` apostrophe is an allowed character in Haskell variable and function names, typically pronounced "prime".When making a copy of a function, it's common to put a `'` at the end of it to differentiate it.) function that uses recursion and pattern matching:

```
length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs  
```

If these remind you of `case` statements in other languages, they should. Pattern matching in Haskell is really just syntax sugar!sidenote(Syntax sugar is an addition to a language that is designed to make things easier to read or to express.) for `case` statements.

They tend to look like this:

```haskell
case expression of pattern -> result  
                   pattern -> result  
                   pattern -> result  
```

The `length'` function from earlier could be written with a case statement, like this:

```haskell
length'' :: (Num b) => [a] -> b  
length'' xs = case xs of []    -> 0  
                        (_:xs) -> 1 + length' xs  
```

---

__***Exercises***__:

1) What do you think will happen if we called `factorial` with a negative number? Hint: try running it, but don't hold your breath for an answer

2) What do you think will happen if you call our `neck [1]`? What do you think will happen if you call `head []`?

3) Write a function that gets the first value from a 4-tuple.

---

## If/Then/Else

!newthought(In Haskell), if and else are expressions. They're analogous to the ternary operator from other languages. If you have an `if`, you *have* to have an `else`. Here's an example taken from the [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell/Control_structures):

```haskell
-- describeLetterIfElse.hs
!include(haskelltests/should_compile/describeLetterIfElse.hs)
```
The general structure of an if/then/else expression is:

```haskell
if [condition] then [value if condition is True] else [value if condition is False]
```

Be careful to have proper indentation - it can lead to compile errors if you don't!sidenote(The general rule is, if you make a newline when writing an expression, the following code should be indented further than the beginning of that expression. See the [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell/Indentation) for more details.).

## Guards

!newthought(Guards are) like if/then/else, only more readable if you have a lot of branches. Here's the above `describeLetter` function implemented with guards (notice that you **don't** use a `=` sign to define a function if you're using guards).

```haskell
describeLetter :: Char -> String
describeLetter c
   | c >= 'a' && c <= 'z' = "Lower case"
   | c >= 'A' && c <= 'Z' = "Upper case"
   | otherwise            = "Not an ASCII letter"
```

`otherwise` is a value that is equal to `True`, being used as a catch-all here. `True` could be used instead, but `otherwise` is more readable. Just like with patterns, guards are evaluated top to bottom, so don't put the catch-all at the top. And be careful to line up your guards correctly, because it looks better but also because your code won't compile if you don't.

## Let Expressions and Where Statements

!newthought(When writing) a function, sometimes it's nice to be able to assign names to intermediate values. There are two ways to do that in Haskell: `let` Expressions and `where` statements. 

1) `let` expressions!sidenote(These are true expressions - you can use them anywhere Haskell expects an expression.) - these take the form of `let [variable bindings] in [expression]`. Here's an example:

    ```haskell
    -- computeEnergy.hs
!indent(!include(haskelltests/should_compile/computeEnergy.hs))
    ```

    You can have multiple bindings in one `let` expression, and use bindings in the definition of other bindings.

    ```haskell
    -- makeURL.hs
!indent(!include(haskelltests/should_compile/makeURL.hs))
    ```

    As you may have noticed, the order of the bindings doesn't matter. Since these are expressions, you can use them anywhere, not just in functions.

2) `where` statements - these aren't expressions, they're statements, and can only be used in functions!sidenote(And a few other places, but functions are where you'll see them most often). They're useful because they work perfectly with guards, where let expressions don't really.

    ```haskell
    -- benchPressJudger.hs
!indent(!include(haskelltests/should_compile/benchPressJudger.hs))
    ```

    You might notice something looks strange about our type signature - what's that `(Ord a, Fractional a)`? It means whatever type `a` is has to be a member of `Ord` (so that we can see if it's larger or smaller than stuff), `Num` so we can multiply it by stuff, and `Fractional` (so we can use it with fractional numbers when converting pounds into kilograms). Not all fractional numbers have to be orderable, that's why we needed to have the `Ord` constraint as well!sidenote(To avoid this in your own code, there are some specialized typeclasses such as `RealFloat`, but `(Ord a, Fractional a)` is perfectly readable even if it feels somewhat silly.).

    Also, pattern matching works almost anywhere you see a `=` sign. So this would have worked, too, if we were very interested in terseness:

    ```haskell
    where benchPressKgs = benchPressLbs * 0.4535
          (weak, decent, strong) = (67, 125, 250)
    ```
    

# Fixing Errors

!newthought(You need) some practice reading compile errors because you'll be doing it a lot. Luckily, the time you lose in fixing a battery of compile errors, you gain by removing many opportunities for runtime errors! Not all of these examples are fair because some are issues we haven't told you about. That's why you have to actually run them, to get good practice! The answers are at the bottom of the section

1) Run this in GHCi: 

    ```haskell
    Prelude> area x = .5 * 3.14 * x^2
    ```

    You should see something like:

    ```haskell
    <interactive>:78:10: error: parse error on input `.'
    ```

    Where I have `78`, you'll have a different number. That tells you the line number the error was on, but in GHCi, it tells you the number of lines you've entered before now. We can ignore that since it isn't super useful.
    The number after the colon (in our case, `10`) is the column the error was on. This is useful because that should be fairly close to the source of the mistake. 10 letters into our command is the start of `.5`. Can you figure out the error? 


2) Make a new file named `errorTest1.hs`. Inside it, write:

    ```haskell
    -- errorTest1.hs
!indent(!include(haskelltests/error_examples/errorTest1.hs))
    ```

    Then try to load it into GHCi with `:load errorTest1.hs`. It should give you an error like the following:

    ```haskell
    errorTest1.hs:5:6: error:
        parse error on input `='
        Perhaps you need a 'let' in a 'do' block?
        e.g. 'let x = 5' instead of 'x = 5'
      |
    5 |  bar = a - a
      |
    ```

    Can you spot the error? The error message isn't very helpful here.

3) Now that you got that figured out, lets do one last one. Make a file called `errorTest2.hs`. Inside it, put:

    ```haskell
    -- errorTest2.hs
!indent(!include(haskelltests/error_examples/errorTest2.hs))
    ```

    Then go to GHCi and run `:load errorTest2.hs`. You should see a bunch of errors that say 

    ```haskell
    errorTest2.hs:2:7: error: Variable not in scope: a :: a -> a
    ```

    Can you find the issue?

4) Just one more, this is probably one of the most common errors you'll get. Try writing this:

    ```haskell
    -- errorTest3.hs
!indent(!include(haskelltests/error_examples/errorTest3.hs))
    ```

    You should see an error like:

    ```haskell
        ..\error_examples\errorTest3.hs:2:21: error:
        * Could not deduce (Ord a) arising from a use of `>'
        from the context: Num a
            bound by the type signature for:
                    isGreaterThan :: forall a. Num a => a -> a -> Bool
            at ..\error_examples\errorTest3.hs:1:1-42
        Possible fix:
            add (Ord a) to the context of
            the type signature for:
                isGreaterThan :: forall a. Num a => a -> a -> Bool
        * In the expression: x > y
        In an equation for `isGreaterThan': isGreaterThan x y = x > y
      |
    2 | isGreaterThan x y = x > y
      |                     ^^^^^
    ```

    Hint: Make sure to read the error message carefully!marginnote(Which is good advice generally, but especially here.).

And remember, a good first step is always to just Google the error!

Answer 1: You can't write a floating point number like `.5`, it needs to be `0.5`. 

Answer 2: All declarations must start in the same column in Haskell. Having some of them being offset will make Haskell think you're trying to do something strange. Remove the space in front of the two `bar`s.

Answer 3: We have `foo = a + a`, but we need to have `foo a = a + a`. Otherwise, Haskell has no idea where `a` is coming from. Same issue for `bar`.

Answer 4: Our only type constraint on our function is `Num a`, but we use `>`, which is given to us by `Ord`, not `Num`!marginnote(This commons source of confusion stems from the fact that not all numbers can be ordered, like complex numbers.).

# Problem Solving Practice

!newthought(None of) these problems are particularly difficult, but they are very mathsy, which may not be your cup of tea. But this chapter will hone your Haskell skills - try to solve the problems yourself! They're well within your abilities by now.

1) Add up all the natural numbers between 1 and 1000!marginnote(Natural numbers are just numbers used for counting, such as 1, 2, 3, etc.), inclusive. 

    1) Hint! Reread the chapter on lists if you're stuck, especially the chapter on enumerations.
    
    2) The answer found by your program should be 500500

2) Add up all the natural numbers that are divisible by 3 or 5, between 1 and 1000 

    1) List comprehensions will come in useful here.

    2) The answer found by your program should be `234168`

3) Write a function that takes a List of `Num`s, and tells you the difference between the sum of the squares and the square of the sums!sidenote(If you're wondering how to find the sum, Haskell includes a `sum` function for that exact purpose. Use List comprehensions to square all the numbers before you sum them, too.). 

    1) For `[1,2,3,4,5]`, my program got `-170`. If yours gets `170` then you just put the terms in the opposite order, which is fine. 

# Functions First Class

## Currying

!newthought(Functions in) Haskell are values like any other!marginnote(This is an important part of being a *function*al programming language.), and can be passed around and all sorts of other things. Observe:

```haskell
Prelude> adder a = a + 1
Prelude> adder 3
4
Prelude> otherAdder = adder
Prelude> otherAdder 3
4
```

See? We've assigned the value of `adder` to `otherAdder`. We should take a look at the types here.

```haskell
Prelude> :type adder
adder :: Num a => a -> a
Prelude> :type otherAdder
otherAdder :: Num a => a -> a
```

Yep, exactly the same. `otherAdder` is now a function that's exactly the same as `adder`.

That brings us to of one of the most famously complicated things in Haskell: Currying!marginnote(Haskell was actually named after the mathematician Haskell Curry.). The general idea is that functions in Haskell always take either 1 or 0 arguments!marginnote(There is an argument to be made that functions in Haskell always take one argument, and that what I previously described as nullary functions are actually just values. However we think that saying "everything is a function" is more intuitive.). When you write a function that seems to take multiple arguments, Haskell converts it to a function that takes one argument, then returns a new function which takes the next argument, etc. That probably seems horribly complicated, So let's see an example. Do you recall the `take` function? 

```haskell
Prelude> :type take    
take :: Int -> [a] -> [a]
Prelude> take 3 "abcdefg"
"abc"
```

`take` is a function that takes two arguments. But if you only supply it with one argument, what you get is a *partially applied function*!sidenote(That's what they're called in other languages, anyway.), which needs one more argument to complete.

```haskell
Prelude> :t take 3
take 3 :: [a] -> [a]
```

Essentially, `take 3` returns a function that takes a list and returns the first 3 values of that list. We can use this function wherever we would write `take 3`. Here's an example:

```haskell
Prelude> myTake3Function = take 3
Prelude> myTake3Function "abcdefg"
"abc"
Prelude> :type myTake3Function
myTake3Function :: [a] -> [a]
Prelude> :type (take 3)
take 3          :: [a] -> [a]     -- Spaces for clarity.
```

This also explains why function type signatures look odd. Remember the type signature for `take`.

```haskell
take :: Int -> [a] -> [a] 
```

Well, that would be more clearly expressed like this:

```haskell
take :: Int -> ([a] -> [a])
```

`take` is a function that takes an `Int`, and returns a function that takes a `[a]` and then extracts the first few values from it. When you write `take 3`, *you're getting a new function that performs `take 3`*. It's really that simple!

```haskell
Prelude> :type (take 3)
take 3 :: [a] -> [a]
```

You don't have to think about this at all when writing functions, this is something Haskell gives you for free. This may seem confusing now, but will become more clear when you see more examples. Remember `concatWithSpace` from a few chapters ago?

```haskell
Prelude> concatWithSpace x1 x2 = x1 ++ " " ++ x2
Prelude> concatWithSpace "Hello," "World!"
"Hello, World!"
```

Let's use currying to make a function that says "Hello" to anything.

```haskell
-- Use currying to make a function that takes a string and puts "Hello " in front of it.
Prelude> sayHello = concatWithSpace "Hello"
Prelude> sayHello "puppy!"
"Hello puppy!"
Prelude> sayHello "house!"
"Hello house!"

-- Now, let's check out our types.
Prelude> :type concatWithSpace
concatWithSpace :: [Char] -> ([Char] -> [Char])    -- Parentheses for clarity
Prelude> :type sayHello
sayHello        ::            [Char] -> [Char]     -- Spaces also added for clarity.
```

!newthought(Currying also) works with operators, but often requires surrounding them in parentheses!sidenote(Which also turns them from infix functions to regular functions.).

```haskell
Prelude> addOne = (1+)
Prelude> addOne 5
6

-- You can curry either side of an infix function.
Prelude> addOne' = (+1)
Prelude> addOne' 5
6
```

Let's rewrite `concatWithSpace` to use currying. 

```haskell
Prelude> concatWithSpace' x1 = ((x1 ++ " ") ++)
Prelude> concatWithSpace' "hello" "curry"
"hello curry"
```

## Map

!newthought(There's an) incredibly useful function in Haskell named `map`. It does something that is quite common in Haskell, but it could be surprising. It has two parameters. The second is a List, but the first one is... another function!sidenote(If you're familiar with functions as parameters from C++, you'll find in Haskell they're much more convenient and type-safe.)! Let's take a look at the `map`'s type.

```
Prelude> :t map
map :: (a -> b) -> [a] -> [b]
```

Let's break this down.

```haskell
   map :: (a -> b) -> [a] -> [b]
--        ========    ===    ===
--           [1]      [2]    [3]
```

1) A function that takes an `a` and returns a `b`!sidenote(In the future, I might call this "a function from `a` to `b`".).

2) A List of `a`s.

3) A List of `b`s.

Let's put it into practice! What it does will become clear.

```haskell
-- Take a look at the type of the function (+1).
Prelude> :t (+1)
(+1) :: Num a => a -> a
-- So it's a function that takes a value of type a and returns a value of type a, where a is a Num.

Prelude> map (+1) [1,8,4,1,-4]
[2,9,5,2,-3]

-- By the way when something is expecting an (a -> b) function,
-- an (a -> a) function works fine.
-- a and b can be different, but they don't have to be.
```

What's happened here is `map` has iterated over every element of `[1,8,4,1,-4]`, and applied the function `(+1)` to it. If you'll look,`1` became `2`, `8` became `9`, etc.

```haskell
Prelude> map (+1) [1,8,4,1,-4]
--                [1,     8,     4,     1,     -4]
--                (+1)   (+1)   (+1)   (+1)   (+1)
--                [2,     9,     5,     2,     -3]
```

However, there's another function called `fmap`, and it's preferred that you use `fmap` over `map`. `fmap` works in situations `map` doesn't, so it's a bit nicer!sidenote(Specifically `map` works only on Lists, but `fmap` works on anything in the typeclass `Functor` (which includes Lists). Unfortunately `Functor`s were developed after `map` so we're stuck with the `f` in front.).

## Filter

!newthought(Lets look) at another function, `filter`. 

```haskell
Prelude> :t filter
filter :: (a -> Bool) -> [a] -> [a]
--           [1]         [2]    [3]
-- 1) function from a to bool
-- 2) List of a
-- 3) List of b

Prelude> isEven x = x `rem` 2 == 0
Prelude> isEven 1
False
Prelude> isEven 2
True

Prelude> :t isEven
isEven :: Integral a => a -> Bool
-- Looks like isEven is a function from a to bool...

Prelude> filter isEven [1..10]
[2,4,6,8,10]
```

See what happened? `filter` "filtered" out the values for which `isEven` returned `False`!

Often with `filter` you need a function fast, but don't feel like giving it a name. For that, it's a good time to use lambdas!sidenote(Also known as anonymous functions.), which look like this: 

```haskell
   \i     ->   i < 4
-- [1]   [2]    [3]
```

1) `\i` - A backslash!marginnote(Backslash was chosen because looks kind of like the Greek letter `λ` lambda, which you may recognize from Haskell's logo.), followed by all the parameters used by your function. In our case we only have one, `i`.

2) `->` - An arrow used to separate the two halves of the lambda

3) `i < 4` - The body for your lambda - in our case, it returns a `True` if the number given is less than `4`, and `False` otherwise.!marginnote(If you want, you can give Haskell some type information about your lambdas, too.

    ```haskell
    Prelude> bangBetween = (\x y -> x ++ "!" ++ y) :: String -> String -> String
    Prelude> bangBetween "Bang" "Bong"
    "Bang!Bong"
    ```

Here's how we'd use it with `filter`:

```haskell
Prelude> filter (\i -> i < 4) [1..10]
[1,2,3]
```

Note that, because of the magic of currying, we could have written that as simply:

```haskell
Prelude> filter (<4) [1..10]
[1,2,3]
```

`filter` is useful for reducing the contents of a list to only contain what you want, and it's used quite a bit!sidenote(As you may have noticed, everything you can do with `map` and `filter` you can also do with list comprehensions. In fact, list comprehensions are converted to `map` and `filter` during compilation!).


## Function composition

!newthought(Function composition) is mainly used to make code more readable, instead of being a huge mess of parentheses. It very common, so it's symbol is just the `.` period.!sidenote(A period also looks a bit like the function composition operator in mathematics, which is the real reason it was chosen.)

The `.` operator is mainly useful for chaining together functions. This is its type:

```haskell
Prelude> :t (.)
(.) :: (b -> c) -> (a -> b) -> a -> c
--        [1]         [2]      [3]  [4]
```

1) First, it takes a function from `b` to `c`.

2) Then, it takes a function from `a` to `b`.

3) Then, it takes a value of type `a`.

4) Lastly, it returns a value of type `c`.

Let's write `.` ourselves. It's actually extremely simple.

```haskell
Prelude> (.) f g x = f (g x)
```

Here `f` and `g` are functions. You can use them just like any other function like we do with`(g x)`. 

So let's see it in action. Here's an example of how we used to do things. We'll make a function that multiplies numbers by 2 and a function that adds 1.

```haskell
Prelude> times2 = (*2)
Prelude> plus1 = (+1)
Prelude> times2plus1 x = plus1 (times2 x)
Prelude> times2plus1 4
9
```

Now, let's rewrite `times2plus1` to use `.` instead!sidenote(The `.` operator might not seem very useful now, but it's extremely common so it'll pay to learn how it works.). 

```haskell
Prelude> times2plus1' = plus1 . times2  -- Use currying for conciseness.
Prelude> times2plus1' 4
9
````

*Function composition* takes two functions as arguments. Then it returns a new function, which just applies the right function!marginnote(The interesting thing about this is that about half of people think it's backward - the function on the right is applied first, then the function on the left, but some people's intuition says it should be the other way around. Remember, it goes ⬅️this way⬅️) to whatever input it gets, then applies the left function.

You can chain it up as much as you want:

```haskell
Prelude> plusABunchThenTimes2 = times2 . plus1 . plus1 . plus1 . plus1 . plus1 . plus1 . plus1
Prelude> plusABunchThenTimes2 5
24
```

Note that this will not work:

```haskell
Prelude> times2 . plus1 3
-- error!
```

The issue is that `plus1 3` is getting evaluated first, and turning into `4`. Then it sees `times2 . 4`, and that doesn't make any sense, since 4 isn't a function. This is what you have to do instead:

```haskell
Prelude> (times2 . plus1) 3
8
```

If all those parentheses seem like they could get out of hand, read on!

## Function Application

!newthought(*Function application*) is something you normally get when you don't do anything!sidenote(You could make a joke about function application being lazy.). It's what happens when you do `head [1,2,3]` - the function `head` is *applied* to `[1,2,3]`. But sometimes it happens before you want it to. That's where `$` comes in. It's a function, and here is its type:

```haskell
Prelude> :t ($)
($) :: (a -> b) -> a -> b
```

It takes a function, and an argument, and applies the argument to the function. We could define it like this!sidenote(To make it complete, we'd also have to set the precedence with the `infixl` keyword.).

```haskell
Prelude> f $ x = f x
```

And here's how we'd use it:

```haskell
Prelude> head [1,2,3]
1
Prelude> head $ [1,2,3]
1
```

You may have noticed it doesn't seem to have changed anything. Well, that's because `$` doesn't really change anything. But it has one very useful property: it has very low precedence. This means it will happen after most other stuff has gotten done. Remember this issue in the last section? 

```haskell
Prelude> times2 . plus1 3
-- error!
```

`plus1` was getting applied to `3` too soon. We wanted to wait until `times2 . plus1` happened, then apply the result!marginnote(As a reminder, the result of `times2 . plus1` is a function which adds one, then multiplies by two.) of that to `3`. We can make this dream a reality with `$`.

```haskell
Prelude> times2 . plus1 $ 3
8
```

Success! Let's try a more complicated usage:

```haskell
Prelude> sum . take 3 $ [1..1000]
6
```

This may seem confusing, but so let's add some parentheses to try and make it more clear:

```haskell
Prelude> sum . (take 3) $ [1..1000]
6
```

`take 3` returns a function that takes 3. It then gets composed with `sum`, which sums all the values of a List, to make a new function that applies `take 3` then applies `sum`. This function is then applied to `[1..1000]`, which results in `6`!sidenote(Note that due to Haskell's laziness, this list is never actually fully evaluated or put into memory, it is only ever evaluated up to the 3rd item.). Let's try really going crazy with it.

```haskell
Prelude> sum . (drop 2) . fmap (+1) . (++[1,2,3]) $ [-1,0]
9
```

As you can see, we use `.` between all our functions, then `$` to the value we want to apply it to. This takes the list `[-1,0]`, adds `[1,2,3]` to it, adds `1` to every number in the list, removes the first two items from the list, and then gets the sum of that list!sidenote(You typically would want to use `let` or `where` to make an intermediate list instead of putting them all into one giant function composition chain, just to make it more readable.).

## Folds

!newthought(There's another) function you should be familiar with: `foldl1`. It's useful when you want to reduce a whole list to a single value.  Let's use `foldl1` to implement `sum'`, a function that takes a list of numbers and sums them all up. We'll need to make a new function, `add`, that takes two numbers and adds them.

```haskell
-- Write a function that takes two numbers and adds them.
Prelude> add x acc = x + acc
Prelude> add 1 3
4

-- Use that function to write sum'.
Prelude> sum' xs = foldl1 add xs
Prelude> sum' [1,3,5,9]
18
```

You're probably wondering how this works. First, notice that `add` takes two values, but only returns one? For example, `add 1 4` evaluates to `5`. What `foldl1` does is use that to take a whole *list* of values and reduce it down to just one. It starts out by using the function to turn the first two values into one value. 

!figure()(/static/wise_mans_haskell/assets/exp_foldl1_step_1.svg)

Notice that the list got shorter! `foldl1` just keeps doing that until there's only one item left.

!figure()(/static/wise_mans_haskell/assets/exp_foldl1_repeat_step_1.svg)

There's another function, `foldr1`, that does the same thing except it goes right-to-left instead of left-to-right. If possible, you should use `foldr1` instead of `foldl1` because `foldr1` is much more efficient. 

---

__***Exercises***__

1) Write a function called `product'` which multiplies the items in the list, instead of adding.

2) Write the `foldl1` function yourself, calling it `foldl1'`. The answer is in the `writefoldl1.hs` file.

    1) Hint! You need recursion.

    2) Hint! The base case is a list with one value. If you get `foldl1' f [x]`, you can just return `x`.

    3) Hint! Check out the chapter on [Recursion Practice](../recursion-practice) if you're stuck.

---

You may have noticed our `add` function, which adds two numbers, is the same as `(+)` (due to currying). So this would also work:

```haskell
Prelude> sum' xs = foldl1 (+) xs
```

And, of course, due to currying we can do this:

```haskell
Prelude> sum' = foldl1 (+)
```

Now, `foldl1` is certainly useful, but it's a bit restrictive. If we have a list of numbers, it has to return a number. If we have a list of characters, it has to return a character, and so on. However, there's the `foldl` which is very similar but allows the output to be a different type than the input. Because of this, you also need to pass it a separate starting value. Let's use `foldl` to write a function that takes a list of numbers and returns a string of all the numbers together. So `[1,2,3]` would be `"123"`. Remember, the `show` function takes a value and returns a `String`, so `show 3` returns `"3"`. We'll also use a lambda to make a function in our expression, instead of defining to separately.

```haskell
Prelude> listString xs = foldl (\acc x -> acc ++ (show x)) "" xs
Prelude> listString [1,2,3]
"123"
```

Here's how this works:

!figure()(/static/wise_mans_haskell/assets/exp_foldl_repeat_step_1.svg)

Of course, there's also `foldr`, which is much faster than `foldl`.

---

__***Exercises***__

1) Rewrite `sum'` using `foldl` instead of `foldl1`. Hint: you can add anything to 0 and get the same number.

2) Write the `foldl1` function yourself. The base case is the empty list. Look at your `foldl1'` function for inspiration.

---

# A Brief Note on Undefined

!newthought(Haskell allows) you to use a special value called `undefined`, also known as `bottom`, also known as `⊥`, also known as `_|_`!sidenote(`_|_` is just an ASCII version of `⊥`. Both are pronounced "bottom").  

Normally Haskell will complain if you use a value of a type it doesn't expect.

```haskell
Prelude> head "hello!"
'h'
Prelude> head True
-- error! head wants an [a], not a bool.
```

But there's a special value in Haskell which is acceptable to all functions, `undefined`!sidenote(Technically, `undefined` is a member of all types.). There's a catch though - if a Haskell program tries to "look at" this value or actually do anything with it, your program will crash.

```haskell
Prelude> i = undefined
Prelude> i + 1
-- error!
Prelude> l = [1,2,3,4,undefined]
Prelude> l !! 3
4
Prelude> l !! 4
-- error!
```

This may seem totally useless. What use is there for a value that you can't use? Well, it comes in handy in a couple of situations, like demonstrating *laziness*. We haven't talked much about laziness, but it's basically the idea that Haskell won't try and compute something it doesn't have to. This is why you can have an infinite List in Haskell - the whole List isn't ever made and loaded into memory, of course. It's just only computed as far along as your program needs. You can demonstrate that the `head` function (which takes a List and returns the first value) doesn't look at any values in the List besides the first one.

```haskell
Prelude> head [1, undefined]
1
Prelude> head [undefined, 1]
-- error!
```

If a function makes your program crash or gets caught in an infinite loop, we say its return value is `⊥`. Imagine we wrote this:

```haskell
Prelude> stupid = sum [1..]
Prelude> stupid
-- infinite loop
```

Trying to evaluate `stupid` will cause the program to hang. You can try to sum an infinite List, but you won't ever finish, you'll just keep adding values forever. So the value of `stupid` is said to be `⊥`!sidenote(This symbol is used because it looks like it's pointing at the bottom of something. We say "bottom" because of some complicated type theory, but it basically means it's "the most undefined" value.). Once your program hits it, it won't do anything else.

Sometimes you want to talk about the fact that you can pass a `⊥` to a function, and it won't crash your program. Here's an example:

```haskell
Prelude> weird x = 3
```

This is a function that takes a value and returns `3`, no matter what you put in. Since Haskell is lazy, it won't try to evaluate `x` until you need it, which this function never will since it throws it away and just returns `3`.

So we could write this, and it'll work just fine:

```haskell
Prelude> weird . sum $ [1..]
3
```

Haskell never needs the value of `sum [1..]`, so it never computes it. But sometimes you want to be able to say this in a way that's a bit more general. That's where `undefined`!marginnote(`undefined` is a good name because, if a function takes your program into an infinite loop, there's no definition for what that functions should return. It'll never return anything!) comes in:

```haskell
Prelude> weird undefined
3
```

Haskell is lazy and so our `weird` function doesn't even look at the value of its parameter `x` (since it doesn't need it). That's good in our case since the value of `x` is `undefined` and looking at it would crash our program.

!newthought(Undefined can) also be a very useful tool. Let's say you're writing a program and you're not really sure how to write a function you need. You can just define it as `undefined` and then you can go work on something else. Your program won't work until you define that function for real, of course, but you can call it elsewhere and Haskell will do its work in making sure your types check out. Newer versions of GHC make this even easier, with what's called a *type holes*. Anywhere GHC expects a value you can add a `_` and GHC will still allow you to compile but gives you a warning that your function won't work at runtime.

# When things might go wrong

!newthought(Often we) want to write a function that just doesn't make sense for all possible inputs. For example, the `head` function!marginnote(`head` extracts the first element from a list.). It works great for most lists, but for empty lists, it crashes your program.

```haskell
Prelude> head [1,2,3]
1
Prelude> head []
-- error!
```

In other languages, what you might do is return some value like `null` or `None`. But those come with issues. For example, what if you forget to check to make sure you got a valid answer before using the value? In fact, this causes so many problems that it's sometimes called [The Billion Dollar Mistake](https://www.infoq.com/presentations/Null-References-The-Billion-Dollar-Mistake-Tony-Hoare). Haskell provides a better way, using `Maybe`. Here's how it works.

```haskell
-- maybeHead.hs
!include(haskelltests/should_compile/maybeHead.hs)
```

We have three new things here. `Maybe a`, `Just` and `Nothing`!marginnote(In case you've forgotten, `(x:xs)` is a pattern that will match any list with at least one element, and set `x` to the first element.). `Maybe a` says that we don't want to return just a regular `a`, we want to return it wrapped up in a `Maybe`. `Maybe a` is a different type than `a`, and you can't return an `a` if your code expects a `Maybe a`. To convert a regular value into a `Maybe` value, we use the `Just` function!marginnote(`Just` takes a value and returns that value wrapped up in a `Maybe` type.). But there's another valid `Maybe a` value, and that's `Nothing`. You can think of `Just` as saying "Give me a value of type `a`, and I will give you a value of type `Maybe a`". Let's play with that a little bit. 

```haskell
*Main> maybeHead ([1,2,3] :: [Int])
Just 1
*Main> maybeHead []
Nothing
```

See? No more crash. But we have an issue: our output isn't an `Int` anymore, it's a `Maybe Int`!

```haskell
*Main> first = maybeHead ([1,2,3] :: [Int])
*Main> :t first
first :: Maybe Int
```

This means we've lost our ability to do Math on it. `Maybe Int` is a different type than `Int`, and we can't add, subtract, multiply, or do anything else to it. However, luckily, `Maybe` plays nice with pattern matching. Let's write a function that takes a `Maybe Int`, and returns a `Maybe Int` with the number increased by `1`. If you pass it a `Nothing`, it'll return `Nothing`. We'll also include our `maybeHead` function from earlier just to have them in the same file.


```haskell
-- maybeAddOne.hs
!include(haskelltests/should_compile/maybeAddOne.hs)
```

Time to see it in action!

```haskell
*Main> first = maybeHead ([1,2,3] :: [Int])
*Main> first
Just 1
*Main> maybeAddOne first
Just 2
```

Let's do that to multiply our `first` by `10`.

You'll notice that we don't have a case for if `first` is `Nothing`. This means our pattern is non-exhaustive and would crash if we provided it a `Nothing`.

```haskell
*Main> first = maybeHead ([] :: [Int])
*Main> case first of (Just x) -> Just (x*10)
--- error!
```


!newthought(We don't) have to be super specific about what types our `Maybe` can use. Remember how we did `Maybe Int`? We could just as easily have changed the type signature to look like this:

```haskell
maybeAddOne :: (Num a) => Maybe a -> Maybe a
```

Which is better because it's more general. If this seems confusing, don't worry, in the next chapter we'll address it more. 

---

__***Exercises***__:

1) Write a function with the type signature:

    ```haskell
    maybeSquare :: (Num a) => Maybe a -> Maybe a
    ```
    
    It should take a `(Just <number>)` and return a `Just (<number>^2)`. If passed `Nothing`, it should return `Nothing`.

    1) So `maybeSquare (Just 4)` should evaluate to `Just 16` 

    2) `maybeSquare Nothing` should evaluate to `Nothing`.

2) Write a function with the type signature `maybePow :: Maybe Int -> Int -> Maybe Int`, that takes a `Maybe Int` and an `Int`, and raises the former to the latter power.

    1) So `maybePow (Just 3) 2` should evaluate to `Just 9`.

    2) `maybePow Nothing 2` should evaluate to `Nothing`.

1) Remember our `neck` function from earlier?

    ```haskell
    neck :: [a] -> a
    neck (_:x:_) = x
    ```
    
    Rewrite it to return a `Maybe a` instead of an `a`, and not crash when provided a list with less than two elements.

---

# Creating New Data Types

## Basic data types

!newthought(You know) by now that Haskell offers a variety of data types, like `Int`, `Integer`, `Bool`, and `Char`. But like most languages, it also allows you to create your own. Remember how `Bool` has two possible states, `True` and `False`? Let's make one a new type `Answer`, that has two states `Yes` and `No`.

```haskell
Prelude> data Answer   =    Yes   |    No
--        [1]   [2]         [3]  [4]   [5]
```

Let's break this down.

1) `data`: This is how we say we're going to declare a new data type!marginnote(In this case, it's what's called a *sum type*, which is similar to an enum in other languages, only better.).

2) `Answer`: This is the name of our data type.

3) `Yes`: This is a possible value our data type can take, like `True` or `False` for the data type `Bool`.

4) `|`: This is telling Haskell we're going to have multiple "constructors" for this data type, which is a fancy way of saying there are multiple ways we can make a value of type `Answer`. In our case, those two ways are typing `Yes` or `No`. This makes it a *sum type* - you can think of it as the sum of multiple different possible values.

5) `No`: This is the other value our data type can take.

!newthought(An important) restriction to note here is that data types have to begin with capital letters. `Person` is a valid data type, `person` is not!marginnote(You might have noticed something quite nice here - a type cannot begin with a lowercase letter, and a function or binding cannot begin with an uppercase letter. This makes it simple to tell the difference between `Foo` and `foo`.).

On Facebook, when you are invited to an event, you can respond "attending", "not attending", or "might attend"!sidenote(Don't let all these enum-ey examples discourage you, we'll get to more complicated uses in a moment.). Let's see how that would be represented in Haskell. 

```haskell
Prelude> data InvitationResponse = Attending | NotAttending | MightAttend
```

If you've tried to play with them. you might have noticed that you don't seem to be able to use these values for very much.

```haskell
Prelude> Attending == Attending
-- error!
<interactive>:4:1: error:
    * No instance for (Eq InvitationResponse)
        arising from a use of `=='
    * In the expression: Attending == Attending
      In an equation for `it': it = Attending == Attending
Prelude> i = Attending
Prelude> i
-- error!
<interactive>:6:1: error:
    * No instance for (Show InvitationResponse)
        arising from a use of `print'
    * In a stmt of an interactive GHCi command: print it
```

Everything seems to be causing errors! That's because there are no functions that we can use on our values with. Let's write some. Make a new file called `invitationResponseFunctions.hs`.

```haskell
-- invitationResponseFunctions.hs
!include(haskelltests/should_compile/invitationResponseFunctions.hs)
```

Now let's load this into GHCi and play with it. To save time, we'll use `:l` instead of `:load`

```haskell
Prelude> :l invitationResponseFunctions.hs 
[1 of 1] Compiling Main ( invitationResponseFunctions.hs, interpreted )
Ok, one module loaded.
*Main> bob = "Bob Vance"
*Main> bobsResponse = NotAttending
*Main> bobsResponse `invitationResponseIsEqual` Attending
False
*Main> bobsResponse `invitationResponseIsEqual` NotAttending
True
*Main> makeMessage bob bobsResponse
"Sorry, Bob Vance can't attend your event."
```

But we still can't use `==` with our data type. That'd be a lot better than using `invitationResponseIsEqual`. Think back to the chapter on typeclasses. Do you remember the `Eq` typeclass? It contains the functions `==` and `/=`. To be able to use those, we have to somehow declare that `InvitationResponse` is a member, or *instance*, of the `Eq` typeclass. To do that, we need to define `==`!marginnote(We get '/=' for free because `Eq` has it defined as `x /= y = not (x == y)` by default, so once you have `==` Haskell can define `/=` on its own.). Here's how we do that.

```haskell
-- invitationResponseFunctionsBetter.hs
!include(haskelltests/should_compile/invitationResponseFunctionsBetter.hs)
```

Let's take a closer look at what's going on there.

```haskell
  instance Eq InvitationResponse where
--   [1]   [2]       [3]          [4]
```

1) `instance`: This is how we tell Haskell that we're saying a type is an instance of a typeclass

2) `Eq`: This is the typeclass we're making `InvitationResponse` an instance of.

3) `InvitationResponse`: This is the typeclass we're making an instance of `Eq`.

4) `where`: This is the same `where` you see in function definitions, it means there's going to be bindings soon (bindings are things like `i = 3`, they're values bound to names).

After `instance Eq InvitationResponse where`, we have our definition of `==`. Let's can import this and test it out, just to prove it works.

```haskell
*Main> :l invitationResponseFunctionsBetter.hs
[1 of 1] Compiling Main ( invitationResponseFunctionsBetter.hs, interpreted )
Ok, one module loaded.
*Main> Attending == Attending
True
```

But this is bad. It's a lot of repetitive typing you'd have to do every time. In software this is called *boilerplate*. Fortunately, Haskell has a built-in way to automatically *derive* `Eq` for us, if we tell it to, using the `deriving` keyword!sidenote(Imagine if you had to do that manually for every data type you made! Haskell gets some criticism of being "for academics", but actually it's usually quite practical.). Let's redefine `InvitationResponse` to use it.

```haskell
Prelude> data InvitationResponse = Attending | NotAttending | MightAttend deriving (Eq)
Prelude> Attending == Attending
True
```

That's much better. We should also add `Show` and `Read`, because they make sense here!sidenote(The typeclasses you can automatically derive are `Eq`, `Ord`, `Enum`, `Bounded`, `Show`, and `Read`.).

```haskell
Prelude> data InvitationResponse = Attending | NotAttending | MightAttend deriving (Eq, Show, Read)
Prelude> show Attending
"Attending"
Prelude> read "Attending" :: InvitationResponse
Attending
```


## Data Types With Other Values

!newthought(Some of) our data types need to be more complex. Sometimes they need to contain other values. This seems strange, but will be clear after a quick demonstration. 

```haskell
Prelude> data Color = White | Black | Red | Green | Blue deriving (Eq, Show)
Prelude> data Residence = House Color | Apartment deriving (Eq, Show)
Prelude> House White
House White
```

We define two data types here. The first is the type `Color`, which can have the value `White`, `Black`, `Red`, `Green`, or `Blue`. We also make the type `Residence`, which can either be a `House` or an `Apartment`. In addition, if it's a `House`, it contains a value of type `Color`!sidenote(This means any time we have a `House`, it also has to have an associated `Color`.). Let's see this in action:

```haskell
Prelude> data Color = White | Black | Red | Green | Blue deriving (Eq, Show)
Prelude> data Residence = House Color | Apartment deriving (Eq, Show)

-- My friend lives in an apartment.
Prelude> myFriendsResidence = Apartment

-- I painted my house green, because it's my favorite color.
Prelude> myFavoriteColor = Green
Prelude> myResidence = House myFavoriteColor

Prelude> myResidence
House Green
Prelude> myFriendsResidence
Apartment

-- We can even have a little fun with it.
Prelude> potusHouse = House White

-- myResidence, myFriendsResidence, and potusHouse are all of type Residence
Prelude> :t myResidence
myResidence :: Residence
```

!newthought(This seems) like a good time to introduce some new terminology.

1) Type constructors: When you write `data Color = [...]` or `data Residence = [...]`, that's you declaring `Color` or `Residence` as a *type constructor*!sidenote(In this case, a type constructor that takes no parameters. Since it takes no parameters, we usually just call it a type. We'll discuss type constructors that take parameters later in this chapter.). Anywhere that Haskell expects a type, you can pass it `Color` or `Residence`. For example, if you had a function called `houseAppraisal` that takes a `Residence` and returns a number representing its value, it's type signature could be `houseAppraisal :: (Num a) => Residence -> a`. `Residence` here is a type constructor, but we'd usually just call it a type.

2) Data constructors: On the other hand, when you write `White | Black | Red | Green | Blue` or `Home Color | Apartment`, you're defining *data constructors*. These are functions and can be used in expressions or pattern matching. For example, the function `warmInSummer (Home Black) = True`. Here, `Home` is a data constructor that takes one argument!marginnote(A data constructor that takes one argument is called a *unary* data constructor.), and `Apartment` is a data constructor that doesn't take any arguments either.!marginnote(Data constructors are sometimes also called *value constructors*.).

Data constructors can take as many data parameters as you want. Let's write a `SongInfo` data type which stores a `Song`, with a `String` for the name of the song, a `String` for the artist, and a `Float` for the length of the song.

```haskell
Prelude> data SongInfo = Song String String Float deriving (Show)
Prelude> Song "Dirty Diana" "Michael Jackson" 4.47
Song "Dirty Diana" "Michael Jackson" 4.47
```

---

__***Exercises***__:

1) Write a data type that has the possible values `A`, `B`, `C`, `D`, and `None`, that you might use to store the user's answer if you were writing a program to administer multiple choice tests.

2) Write some data types used to store information about a car. You should have one data type for `Make` (`Honda`, `Toyota`, `GM`, or `Tesla`) and one for `EngineCylinders` (`Four`, `Six`, and `N/A`). Then make a data type `CarInfo` which stores the `Make` and the `EngineCylinders`, plus a `String` representing the name of the owner. Don't forget to add `deriving (Show)`! This may seem difficult but once you've done it, it won't seem bad at all. 

---

!newthought(Since type) constructors and data constructors are always used in different places, Haskell allows you to have a data constructor with the same name as a type constructor. Take this somewhat useless type:

```haskell
data Bob = Bob
```

This makes a new data type, `Bob`, with one possible value, `Bob`. There's not really a good reason to make this type, but Haskell will still allow you to. `Bob` the data constructor takes no arguments, so it is sometimes called *nullary* or *constant*. Compare it to `House` earlier, which took a `Color` as an argument.

You can also use regular types as parameters. Let's make a `Person` type, with a `String` for his name.

```haskell
Prelude> data Person = Person String deriving (Eq, Show)
Prelude> Person "Andy"
Person "Andy"
```

It'd also be useful to have a function that can extract the name from a `Person`. Let's write one using pattern matching.

```haskell
Prelude> getName (Person name) = name
Prelude> getName (Person "andy")
"andy"
```

But we don't actually have to write the `getName` function yourself, that would be a bit silly. Haskell has a way for you to write a name for each parameter represents, and create the necessary functions automatically. This is called *record syntax*.

```haskell
Prelude> data Person = Person { firstName :: String, lastName :: String } deriving (Eq, Show)
Prelude> jDoe = Person "John" "Doe"
Prelude> jDoe
Person {firstName = "John", lastName = "Doe"}
Prelude> firstName jDoe
"John"
Prelude> lastName jDoe
"Doe"
```

Let's break down record syntax.

```haskell
   Person   {   firstName :: String, lastName :: String    } 
--   [1]   [2]    [3]          [4]     [5]         [6]    [7] 
```

1) `Person`: You can think of this as the name of this specific data constructor!sidenote(Which of course, is exactly what it is. A data constructor is a function and this one's name is `Person`. Data constructors are the exception to the rule that functions must begin with a lowercase letter, as they must also begin with an uppercase letter.).

2) `{`: This is to indicate that we're annotating the types for this data constructor with names using record syntax.

3) `firstName`: This is the name of the first parameter to the data constructor.

4) `String`: This is the type that corresponds to the name `firstName`.

5) `lastName`: This is the name of the second parameter to the data constructor.

6) `String`: This is the type that corresponds to the name `lastName`.

7) `}`: This indicates we're done with this data constructor and record syntax.

Haskell then automatically makes `firstName` and `lastName` functions, which take a `Person` and return that value!sidenote(Again, this is purely for convenience, as you could write all these functions yourself.). 

You'll notice that `firstName :: String` and `lastName :: String` are just regular type signatures. But be careful, `firstName :: String` isn't the type signature of the `firstName` function that's created, it's the type signature of what that function returns (in our case, a `String`)!marginnote(Note that you can't have multiple records with the same name, even in different data types. If this irritates you, in a later chapter we'll see [a GHC feature](https://downloads.haskell.org/~ghc/master/users-guide/glasgow_exts.html#duplicate-record-fields) that loosens this restriction.).



## Type Aliases

!newthought(The) `type` keyword makes an *alias* of a data type!sidenote(also called a *type synonym*). That just means it takes a data type and makes a new name for it. Remember how instead of writing `[Char]`, you can write `String`? That's done with the `type` keyword.

```haskell
type String = [Char]  
```

It doesn't make a new type, just a new name for an existing type!marginnote(Although unfortunately in error messages GHC will use the original names and not your synonyms.). It's just to make our functions easier to read. 

```haskell
-- Compare this:
Prelude> p1 = (3, 3) :: (Double, Double)
Prelude> p2 = (10, -1) :: (Double, Double)

-- To this:
Prelude> type Point = (Double, Double)
Prelude> p1 = (3, 3) :: Point
Prelude> p2 = (10, -1) :: Point
```

Regular types can quickly get hard to manage, but with type aliases, that's somewhat avoided.

```haskell
Prelude> type Point = (Double, Double)
Prelude> type Path = [Point]
Prelude> type Polygon = Path
Prelude> type ComplexPolygon = [Polygon]
```

## Parameterized Types

!newthought(Some types) aren't really their own types, they're like modifications of other types. Let me explain. In the past, we've always seen data declarations like this:

```haskell
Prelude> data Color = White | Black | Red | Green | Blue 
Prelude> data Residence = House Color | Apartment 
```

Here, `House` is a data constructor!sidenote(Which, remember, is a function.) that takes one argument of type `Color`. It then returns a value of type `Residence`. `Apartment` is also a data constructor, which takes no arguments and returns a value of type `Residence`.  `White`, `Black`, `Red`, `Green`, and `Blue` are all data constructors that take no parameters and return a value of type `Color`.

```haskell
Prelude> :t House
House :: Color -> Residence
```

See? `House` is a function that takes a color, and returns a `Residence`!marginnote(`House` on its own is *not* a residence, but `House Green` is.). But type constructors can take parameters, too!marginnote(These are called *parameterized types* or *parameterized type constructors*.). Except their parameters aren't values, they're other types! The most common example is probably the type constructor `Maybe`. Here's how it's defined:

```haskell
data Maybe a = Nothing | Just a  
```

`Maybe` isn't a proper type on its own, it's just a type constructor. It takes a type as a parameter, and then the `a` in `Just a` is replaced with that type. Here's an example. Let's say you want to make a function that takes two numbers and divides the first by the second. But there's not really a good answer when the second value is `0`, because you can't divide by zero. You could use `Maybe` to help write that function:

```haskell
-- maybeDivide.hs
!include(haskelltests/should_compile/maybeDivide.hs)
```

We can load this into GHCi to test it:

```haskell
Prelude> :l maybeDivide.hs
[1 of 1] Compiling Main ( maybeDivide.hs, interpreted )
Ok, one module loaded.
*Main> maybeDivide 3 4
Just 0.75
*Main> maybeDivide 3 0
Nothing
```

`Maybe` isn't a type, it's a type constructor. `Maybe Double` is a type, but you can also call it a *concrete type*!marginnote(Sometimes people call type constructors like `Maybe` types, either by accident or by laziness, but they really mean type constructors.). Let's look at the type of the `Just` data constructor.

```haskell
Prelude> :t Just
Just :: a -> Maybe a
```

It takes a value of type `a` and returns a value of type `Maybe a`, where `a` is a type variable and can be any type!marginnote(To reiterate, types and values are different things in Haskell. `Maybe Double` is a type, suitable for use in type signatures. `Just 4.5` is a value of type `Maybe Double`. A function whose return type is `Maybe Double` could return `Just 4.5`.). Let's look at some more types:

```haskell
Prelude> :t Just "hello"
Just "hello" :: Maybe [Char]
-- Just "hello" returns a value of type Maybe [char]. 
-- This means Just "hello" can be the return value of any function that returns a Maybe [char]

Prelude> :t Nothing
Nothing :: Maybe a
-- Nothing is a value of type Maybe a, so it can be the return value of any function that returns a Maybe <something>.

Prelude> :t Just 4
Just 4 :: Num a => Maybe a
-- Just 4 returns a Maybe a, where a is any type in the typeclass Num. We could use this for a function that was returning a Float, a Double, an Int, etc.
```

These are also called *parameterized types* because their type constructors that take parameters.

!newthought(There's another) very useful parameterized type called `Either`. It's used when a function can return two different types, most often for error handling!marginnote(`Either` has an advantage over `Maybe` when there are multiple reasons an operation can fail, because you can return an error message instead of an uninformative `Nothing`.). Here's how it's defined:

```haskell
data Either a b = Left a | Right b
```

Let's play with Either in GHCi.

```haskell
Prelude> Left "hello"
Left "hello"
Prelude> Right True
Right True

Prelude> :t Left "hello"
Left "hello" :: Either [Char] b

Prelude> :t Right True
Right True :: Either a Bool
```

Parameterized types are hugely important in Haskell, so I want to go over it again here. `Left "hello"` returns a value of type `Either [Char] b`. That means if you're writing a function, and its type signature says it returns a value of type `Either [Char] <anything>`, then you can return `Left "hello"`!sidenote(Recalling, of course, that a `[Char]` is any `String`, like `"hello"`.). `Either` is a type constructor that takes two Types as arguments - one for the type of the `Left`, and one for the type of the `Right`. You can think of `Either` as the opposite of a 2-tuple - you use either when you want to return either this *or* that, you use a tuple when you want to return both this *and* that.

Let's rewrite `maybeDivide` to use `Either` instead. It will return a `String` with an error message on the left if it couldn't do the division, otherwise it will return the result on the right.

```haskell
-- eitherDivide.hs
!include(haskelltests/should_compile/eitherDivide.hs)
```

We can load this into GHCi and play with it.

```haskell
Prelude> :l eitherDivide.hs
[1 of 1] Compiling Main ( eitherDivide.hs, interpreted )
Ok, one module loaded.
*Main> eitherDivide 3 4
Right 0.75
*Main> eitherDivide 3 0
Left "Sorry, can't divide by zero"
```

---

__***Exercises***__:

1) In `data Either a b = Left a | Right b`, find the type constructor(s), and find the value constructor(s). 

    Really try this, because understanding Haskell data types is important.

    Answer: `Either` is the type constructor, which takes to parameters, `a` and `b`. The two value constructors are Left (which takes an `a`) and Right (which takes a `b`).

---


!newthought(Ok, lets) try writing our own parameterized type, it'll represent a shape. We want the user to be able to use any type of number they want in defining our shape, so we'll not specify one. Just for fun, let's also write a function that takes a shape and tells us it's area!sidenote(A better way would be to make a `Shape` typeclass, but this works too.).

```haskell
-- shapeTypes.hs
!include(haskelltests/should_compile/shapeTypes.hs)
```

Notice the parentheses when pattern matching in the `shapeArea` function. They're there to ensure Haskell knows that `(Circle (x, y) radius)` is all supposed to be one pattern, so Haskell knows that `radius` is part of `Circle` and isn't just another parameter. The same goes for `(Rectangle (x1, y1) (x2, y2))`. Without the parentheses, this code wouldn't compile. 

Let's load this into GHCi and play with it.

```haskell
Prelude> :l shapeTypes.hs
[1 of 1] Compiling Main ( shapeTypes.hs, interpreted )
Ok, one module loaded.
*Main> myShape = Circle (2, 2) 5
*Main> shapeArea myShape
39.25
```

Yep, looks like it's working well! To really drive the point home, let's look at the type for the data constructor `Circle`.

```haskell
*Main> :t Circle
Circle :: (a, a) -> a -> Shape a
```

So `Circle` is a data constructor that takes a tuple of two values of type `a`, and another value of type `a`, and returns a value of type `Shape a`.

You might have noticed that in our definition for `Shape` we never say the type `a` has to be in the typeclass `Num`. We could have, but it's considered bad practice in Haskell. That's because we'd have to duplicate the `(Num a) =>` in every function that we made take a `Shape a`, even if it wasn't relevant for that function. By not adding it, we have the option to only put `(Num a) =>` on the functions that really need it!sidenote(In fact, putting type constraints in `data` definitions is considered such bad practice that there are ways to make GHC warn you when you do it.).

You can also parameterize type aliases. For example, we used `(a,a)` a few times in that definition - let's try and make that clearer with type aliases.

```haskell
type Point a = (a, a)
data Shape a = Circle (Point a) a | Rectangle (Point a) (Point a) deriving (Show)  
```

In fact, let's use record syntax to make it even clearer.

```haskell
*Main> type Point a = (a, a)
*Main> data Shape a = Circle {center :: Point a, radius :: a} | Rectangle {corner1 :: Point a, corner2 :: Point a} deriving (Show)
*Main> myShape = Circle (3,5) 10
*Main> radius myShape
10
```

This could now be a production-ready shape class!

## Kinds

!newthought(Let's try) and make this clearer by talking about *kinds*. We discussed earlier that there are two kinds of types. There are concrete types like `Int` or `[Char]` or `Maybe Int` or `(Num a) => Maybe a`, and not-real-types like `Maybe` and `Either` or even `Either Int`!sidenote(Remember, `Either` needs two values, such as `Either Int Int`. If you only provide one, you haven't got a concrete type.). The type constructors in the second group still need something else to turn into a concrete type that we'd be allowed to use in a type signature. `Int` and `Maybe Int` are concrete types - `Maybe` still needs another type to make it a concrete type, so you can't use it in a type signature. We can see this by using the `:kind` or `:k`!marginnote(Most GHCi commands let you just use the first letter instead of typing out the whole word, like `:load` and `:l` or `:type` and `:t`.) commands in GHCi.

```haskell
Prelude> :k Int
Int :: *
```

The `*` means it's a concrete type!marginnote(In the future, `*` will be renamed to `Type`. If you're using GHCi and seeing `Type` instead of `*`, it means you may be reading an out-of-date version of this book.). You could pronounce it like "star" or just "concrete type". Let's look at some other concrete types.

```haskell
Prelude> :k [Char]
[Char] :: *
Prelude> :k Maybe Int
Maybe Int :: *
```

See? They're all concrete types. 

Let's look at some type constructors instead, like `Maybe`.

```haskell
:k Maybe
Maybe :: * -> *
:k Maybe Int
Maybe Int :: *
```

Ah, see? `Maybe` needs another type as a parameter before it can be a concrete type. You can read this as "The type constructor Maybe takes a concrete type and returns a concrete type". Let's look at `Either`.

```haskell
Prelude> :k Either
Either :: * -> * -> *             -- Either is a type constructor that takes two concrete types and returns a concrete type
Prelude> :k Either String
Either String :: * -> *           -- Through Currying, we can see the value of Either String. It still needs another concrete type 
Prelude> :k Either String Double
Either String Double :: *         -- Either String Double is finally a concrete type that we could use in a type signature!
```

When Haskell wants a concrete type, any concrete type will work, even one made from other type constructors.

```haskell
Prelude> :k Maybe (Either String Double)
Maybe (Either String Double) :: *
```

`Either String Double` is a `*`!marginnote(Remember, `*` means concrete type.), and `Maybe` takes a `*` and returns a `*`, so `Maybe (Either String Double)` is a concrete type. You may have noticed that lists seem like they're just a modification of another type, like how an `[Int]` is just a List of `Int`s. Well, `[]` is actually a type constructor! 

```haskell
Prelude> :k []
[] :: * -> *
```

`[]` the type constructor is different from `[]` the data constructor, so don't get them confused. `[Int]` is a concrete type appropriate in a type signature (and is actually syntax sugar for `[] Int`), `[1]` is a value (and is syntax sugar for `1:[]`).

!newthought(But when) is learning about kinds useful? Well, let's talk about the `Functor` typeclass. The `Functor` typeclass is for things that can be mapped over. You might remember the `map` function, which takes a function and a List, and applies the function to everything in the List. 

```haskell
Prelude> map (+5) [1,2,3]
[6,7,8]
```

Well, for historical reasons `map` only works on lists, but there are lots of other things that can be mapped over. In Haskell you can have sets, arrays, fingertrees, and all kinds of other data structures we'll discuss later. These data structures are in the `Functor` typeclass. Because `map` was stuck as only working on lists for historical reasons!marginnote(You'll find that "historical reasons" explains quite a bit about Haskell.), they made `fmap`, which is the same but works on any `Functor`. Here's how it's defined.

```haskell
class Functor f where  
    fmap :: (a -> b) -> f a -> f b  
```

You might be curious what that `f a` is (notice that it's the same `f` from `Functor f`). It's something we haven't seen before. The `f` *represents a type constructor*, and it later an `a` (which can be of any type) and returns a concrete type!marginnote(This means that type constructors can be instances of type classes, not just concrete types. This is very powerful because it lets us make functions that work on all lists, like `fmap`, but also works on other type constructors, like `Maybe` (which we'll see in a moment).)! So `Functor` is a typeclass that applies to type constructors, not complete types. `[]` is an instance of `Functor`, of course. Let's see how that's defined.

```haskell
instance Functor [] where  
    fmap = map  
```

Pretty basic. `fmap` is just the functor typeclass version of the List-specific `map`. When applied to lists, `fmap` only needs to do what `map` already does. Let's try applying it to `Maybe`. `fmap (+1) (Just 4)` evaluates to `Just 5`. `fmap (+1) Nothing` evaluates to `Nothing`. If you're mapping over `Just <something>` you apply the function to `<something>`, if it's `Nothing` you return `Nothing`. Here's it in Haskell. 

```haskell
instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing  
```

Let's try it out.

```haskell
Prelude> fmap (\x -> x * 2 + 1) (Just 1)
Just 3
Prelude> fmap (\x -> x * 2 + 1) (Nothing)
Nothing
```

You can probably see how this would be useful! Imagine we had a function that returned a `Maybe Int`, and we wanted to do some calculations on the `Int` inside. We can do them all with `fmap`s, and wouldn't have to first check if it was really a `Nothing`.

---

__***Exercises***__:

1) Do you remember the `foldr` function? We used it to sum a list with `sum xs = foldr (\x acc -> acc + x) 0 xs`. Here is its type:

    ```haskell
    Prelude> :t foldr
    foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
    ```

    What does the type variable `t` take when folding over a list? Hint: look at the third argument of `foldr (\x acc -> acc + x) 0 xs`.

---



## Making Our Own Typeclasses

!newthought(As you) know, we have access to quite a few typeclasses in Haskell, such as `Eq`, `Ord`, `Show`, and `Num`. Let's see how `Eq` `might be defined!marginnote(Note that this is not the full definition because it lacks the default implementations, which we'll add in a moment.):

```haskell
class Eq a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool  
```

Let's zoom in on this, although it's pretty simple. 

```haskell
   class     Eq a     where  
--  [1]      [2]       [3]
```

1) `class` - This keyword tells Haskell that we're declaring a new typeclass. 

2) `Eq a` - Then comes the name of the typeclass that we're defining, and the type variable that we'll be using.

4) `where` - This keyword should look familiar! It means we're going to be defining bindings soon. Here though, all we've done is declare functions and give their type signature - we don't actually give the definition of the function.

To make a data type an instance of this typeclass, we'd have to use the `instance` keyword!sidenote(see [Basic data types](../basic-data-types) for more). To make a type an instance of a typeclass, a definition for every function declared in the typeclass must be given, unless there is a default implementation. This is done in the `Eq` typeclass, like this.

```haskell
class Eq a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool  
    x == y = not (x /= y)  
    x /= y = not (x == y)  
```

Of course, you must override one of these, otherwise you will find yourself in an infinite loop. Remember the `Show` typeclass? It contains a function, `show`!marginnote(`Show` also has some other functions that aren't relevant here, like `showsPrec`.), which takes a value and returns a `String` which is supposed to represent the value somehow. Let's write our own, `ShowFrench`, which will be similar, but return a representation in French instead of English.

```haskell
-- showFrench.hs
!include(haskelltests/should_compile/showFrench.hs)
```

Now, let's load this into GHCi to test it out!

```haskell
Prelude> :l showFrench.hs
[1 of 1] Compiling Main ( showFrench.hs, interpreted )
Ok, one module loaded.
*Main> showFrench (1 :: Int)
"un"
*Main> showFrench True
"vrai"
```

Seems to work, but if you pass `showFrench` an `Int` greater than `3`, your program will crash!marginnote(Specifically, what will happen is an exception will be triggered, and it probably won't be handled because we haven't learned how to handle exceptions yet.). In those cases, it would be nice to just show the number. Let's use this opportunity to also require that instances of `ShowFrench` must also be instances of `Show`, and then just run `show` on the number if we don't have a French translation. Here's how we'd do that.

```haskell
-- showFrenchBetter.hs
!include(haskelltests/should_compile/showFrenchBetter.hs)
```

Notice how we changed `ShowFrench a` to `(Show a) => ShowFrench a`. This means that our type variable `a` must be an instance of `Show`.

# Recursion Practice

!newthought(We've used) a little bit of recursion in previous chapters, but it's very important!marginnote(Haskell actually doesn't have the primitive `while` or `for` loops you might be familiar with from other languages. All looping behavior must be accomplished with recursion at some level.) so I'd like to get some practice in.

1) Let's write a copy of the `length` function!marginnote(In case you've forgotten, `length` is a function that takes a List and returns its length.). Now, let's think about how we would write this recursively.

    A strategy tip for writing recursive programs is to think about what case is the most basic. For us, it's probably the empty List, which obviously has length zero. Here's how it's done.

    ```haskell
    -- length.hs
    !indent(!include(haskelltests/should_compile/showFrenchBetter.hs))
    ``` 

    This may seem like it's less efficient than a more iterative approach like you'd see in other programming languages. But GHC does something called *tail call optimization*, which means that if a function ends in a recursive call, it will be optimized into something more similar to a *while loop* from other languages!sidenote(Provided you have the correct optimization settings turned on in GHC.). 

2) Let's write a function that will take a List of numbers, and return a List of all those numbers raised to the second power.

    As before, the most basic case for this function is the empty List, because it doesn't have to do anything. We'll also be using the classic `:` operator, which takes a value of type `s` and a List of type `[a]`, and returns a new List with that value at the beginning. So `1:[2,3]` evaluates to `[1,2,3]`, and `1:[]` evaluates to `[1]`!marginnote(You may remember that `[1,2,3,4]` is actually syntax sugar for `1:2:3:4:[]`, so saying "`1:[]` evaluates to `[1]`" is a bit of a simplification.).

    ```haskell
    -- squares.hs
    !indent(!include(haskelltests/should_compile/squares.hs))
    ```

3) Let's write `map :: (a -> b) -> [a] -> [b]`. If you've forgotten, `map` takes a function and a List, and applies the function to every element in the List. So `map (+1) [1,10,20]` evaluates to `[2,11,21]`

    You'll notice that the most basic case (the *base case*, if you will) is usually the empty List, for functions that operate on lists. Here's how we'd write this:

    ```haskell
    -- map.hs
    !indent(!include(haskelltests/should_compile/map.hs))
    ```

    Study this until you grok how it works - it's very similar to the `map` function we wrote earlier.

4)  Let's write a function that iterates over a List of numbers and removes all the even numbers, we'll call it `odds`. 

    To aid with this, we'll use the `rem` function, which divides two numbers and tells us the remainder!sidenote(We could also use the `mod` function, which is similar, but `rem` is faster.). If a number is odd, it's remainder won't be `0`!sidenote(It will either be `1` or `-1`.). `rem` is only available for `Integral`, not `Num`, so we'll have to limit ourselves to that.  

    ```haskell
    -- odds.hs
    !indent(!include(haskelltests/should_compile/odds.hs))
    ```

    What this does is check if the first element in the List is odd, and if so, combine it with `odds` called on the rest of the list. Otherwise, we'll just return `odds` called on the rest of the List. And as usual, if the List is empty, we return the empty List.

---

__***Exercises***__:

1) Write the `filter :: (a -> Bool) -> [a] -> [a]` function, call it `filter'`. It takes a function of type `(a -> Bool)` and a List of type `[a]`, and then *filters* the List by removing the elements where the function returns `false` when that element is passed into it. For example, 

    ```haskell
    Prelude> filter (\x -> x > 5) [1,2,3,4,5,6,7,8,9,10,-20,400] 
    [6,7,8,9,10,400]
    ```

    Hint: Look at our `squares` function and our `map'` function. 

2) Rewrite your filter function using [guards](## Guards).

---

# Newtype

Sometimes you want to make a type that's almost the same as another type. For example imagine our program calls for a `Dollar` type, a `Yen` type, and a `Euro` type, which are all just wrappers around `Double`. And let's say also we had a `Currency` typeclass with a `convertToDollars` and `convertFromDollars` function. We'd like to add, subtract, and multiply our currency like we could regular numbers. 

One way to make our types would be as follows:

```haskell
!include(haskelltests/should_compile/currency_basic_types.hs)
```

Now, this has two issues. The first is that we can't add or subtract two `Dollars`. We'd have to make each of these an instance of the `Num` typeclass, like this.

```haskell
!include(haskelltests/should_compile/currency_with_instance.hs)
```

This is some pretty boring boilerplate - it'd be almost the exact same for every currency! Wrapping one type (in our case, `Double`) in another (in our case, `Dollar`) is such a common need that we have special syntax for it, `newtype`. 

```haskell
!include(haskelltests/should_compile/currency_with_newtype.hs)
```

The main difference between using `newtype` and `data` is that `newtype` only works with the very simple situation of wrapping one type in one other type. You can't use sum types or have multiple types wrapped up in one. And there's a special GHC feature that makes `newtype` much more useful by letting it automatically derive typeclasses for you, and you turn it on by putting `{-# LANGUAGE GeneralizedNewtypeDeriving #-}` at the top of your code. This is called a *pragma* to turn on a *language extension*. We'll discuss these later, for now just know that putting that at the top of a file turns on an extra feature of GHC. Here's how it looks in action:

```haskell
!include(haskelltests/should_compile/currency_with_newtype_derived.hs)
```

With `GeneralizedNewtypeDeriving` turned on, we were able to add `Num` to our list of typeclasses we'd like to be automatically derived, which is very useful! We'd be able to run `(Dollar 3) + (Dollar 4)` to get `Dollar 7.0`. 

There's one other difference between `newtype` and `data`. Specifically, whether the constructor is strict or lazy. Imagine the following:

```haskell
data D = D Int
newtype N = N Int
```

Now, you may remember that Haskell tries to only evaluate things when really necessary, so if you write `1+2` it won't actually evaluate that until it needs to. Haskell also has a special value named `undefined` which you can pass to any function and causes your program to instantly crash when it's evaluated. 

```haskell
Prelude> undefined
-- Error!
```

But, it won't evaluate it until it has to.

```haskell
Prelude> a = undefined
-- Works fine, until we try to use a
```

You can even pass it to a function, and if the function doesn't look at it your program will still run.

```haskell
Prelude> a = undefined
Prelude> silly v = "silly"
Prelude> silly a
"silly"
```

Now, we'd like our `Dollar` to function identically to a `Double`. But if we defined it with `data`, it wouldn't! Let's see how.

```haskell
Prelude> sillyD (Dollar v) = "silly"
Prelude> sillyD undefined
-- error!
```

We could run `silly undefined` without crashing, but we can't for `sillyD undefined`! However, this works as expected with `newtype`.

```haskell
Prelude> sillyD (Dollar v) = "silly"
Prelude> sillyD undefined
"silly"
```

This difference is somewhat technical and academic, it's worth discussing. To reiterate:

1) `data` is for making new, complicated types, like `data Person = Bob | Cindy | Sue`.

2) `newtype` is for "decorating" or making a copy of an existing type, like `newtype Dollar = Dollar Double`.

3) `type` is for renaming a type, like `type Polygon = [Point]`, which just makes `Dollar` be equivalent to `Double` and is mostly only used for making certain code easier to read.

# Writing Real Haskell Programs

!newthought(There are) a few things you must be able to do before you can write real Haskell programs. You need to be able to write a program, compile it, and run it. You need to be able to make this program do things that contain the dreaded *side-effects* - stuff that pure functions can't do, like read files or write text to the terminal. And, most importantly, you need a stylish environment to develop your Haskell in!

## Environment

!newthought(For developing) Haskell, there are many excellent options. There's [VSCode](https://code.visualstudio.com/) with [Haskero](https://marketplace.visualstudio.com/items?itemName=Vans.haskero) (what I personally use), [Spacemacs](http://spacemacs.org/) (which is great if you're used to Emacs or Vim), and [Emacs](https://www.gnu.org/software/emacs/) with [Dante](https://github.com/jyp/dante) are common options. Extensions also exist for Atom and Sublime Text. I will be assuming you use the same setup as me, VSCode and Haskero. 

In VSCode I find the `"files.autoSave": "onFocusChange"` setting to be invaluable, as it will mostly keep your files saved without too much trouble This also works well with some GHC options that automatically rebuild your code every time it changes, more on that later. I recommend checking if your favorite text editor has something similar.

## Stack Projects

## Definitions

First some quick names you might not be familiar with. 

1) **GHC** is a Haskell compiler. It reads your Haskell source code and *compiles* it into a form readable by a computer. There are many other Haskell compilers, but GHC is the best!marginnote(It's somewhat unfortunate that there's only one popular Haskell compiler, since it's not without issues. But in general it's very nice, especially with all the nonstandard but very useful language extensions it contains.). You got it when you installed the Haskell platform.

2) **Cabal** is a file format for describing certain information about your project. There's also a tool called Cabal, but we won't be using it. Well, we'll be using Stack, which uses Cabal in the backend. The file you describe your package in is normally called `projectName.cabal`!sidenote(Obviously, `projectName` replaced with the name of your project.).

3) **Stack** is a program for managing the packages you use in your project. It is mostly used to aid in using 3rd party code and installing the correct version of GHC. It has a file called `stack.yaml`, but also reads `<projectName>.cabal`, and confusingly a file named `package.yaml` that overrides `<projectName>.cabal`. An important goal of Stack is that if it works on my machine, it should work the exact same way on yours, too. This is called *reproducible builds*!marginnote(The goal actually is that it creates the exact same binary, so it's *guaranteed* to run the same way.), and is a very helpful property. You already got Stack when you installed the Haskell platform.

## Creating a Project

!newthought(Stack contains) many useful features. One is that it allows you to instantly create projects according to some [common templates](https://github.com/commercialhaskell/stack-templates). The one we will use is `simple`. In a terminal, navigate to your development directory and run the following.

```haskell
stack new project1 simple -p "author-name:Andre Popovitch"
```

This will make a new project called `project1` according to the `simple` template, and in the licence text it will attribute everything to me!marginnote(There are a number of other parameters you can use as well. They are `author-email`, `author-name`, `category`, `copyright`, `year` and `github-username`.)! Of course, you can change this to your name if you'd prefer I don't have the copyright over all the code in your project. After running this, you'll see a lot of output in the terminal. Hopefully it'll look something like this:

```haskell
λ stack new project1 simple -p "author-name:Andre Popovitch"
Downloading template "simple" to create project "project1" in project1\ ...

[...]

Selected resolver: lts-12.8
Initialising configuration using resolver: lts-12.8
Total number of user packages considered: 1
Writing configuration to file: project1\stack.yaml
All done.
```

And here what the contents of the `project1` folder should be!marginnote(This uses the `cd` command and the `ls` command. `cd` is short for "change directory", and allows you to navigate to a different folder (in our case, the one we just made). `ls` is short for "list", and lists all the files and folders in the folder we're currently in.):

```haskell
λ cd project1\
λ ls
LICENSE  README.md  Setup.hs  project1.cabal  src  stack.yaml
```

!newthought(First on) the List is the `LICENSE` file. Open it in your favorite text editor (mine is VSCode) and take a look. If you plan to share your code, this license bears looking at. If you don't care who uses it, the current license is fine. Otherwise, check out [ChooseALicense](https://choosealicense.com/) to pick one that works for you. If you don't want anyone to use your code, just replace the contents of this file with the text `no license provided, all rights reserved`.

!newthought(Next up) is the `README.md` file. This is used by other programmers or possible users of your software to determine what it's purpose is and how to use it!sidenote(It's also the file that will be displayed front-and-center if you ever decide to share your project on [Github](https://github.com/) or [Gitlab](https://about.gitlab.com/).). If you intend on sharing your code, it's wise to put some information about your project here. Even if you don't plan on sharing, this can be a useful place to leave notes to your future self.

!newthought(Following that) begins the real meat of our project. `Setup.hs` contains only two lines.

```haskell
-- Setup.hs
!include(haskelltests/projects/project1/Setup.hs)
```

The purpose of this code is somewhat complicated to explain right now, just know that most projects shouldn't have to modify it or really care about what it does. 

!newthought(Next up) is `project1.cabal`. Remember the `.cabal` files mentioned earlier? This is one of those. Let's see what it contains.

```yaml
!include(haskelltests/projects/project1/project1.cabal)
```

Most of this is just data presented in the machine-readable format YAML. so someone could search a database for all projects written by `Andre Popovitch` or similar. This is also where we'd add project dependencies.

!newthought(Next is) the `src` folder, which only contains one file, `Main.hs`. This is where we'd start writing our actual program. Here are the contents of `Main.hs`. 

```haskell
-- Main.hs
!include(haskelltests/projects/project1/src/Main.hs)
```

Every single line of this program contains something we haven't talked about yet. Let's go over them now.

1) `module Main where`: This defines a new *module* called `Main`, which contains the full contents of the file below it. Modules are collections of related code - they're nice because if you break your project into small modules, you can reuse them in future projects or share them with the world!marginnote(There are many sites where you can share your code, but the largest is [Hackage](https://hackage.haskell.org/). It's often a good idea to search here if you're looking for third-party code, just beware because much of it is somewhat low quality.) for fame and fortune. 

2) `main :: IO ()`: This is the type signature for the function `main`. Its type is `IO ()`, which seems like a very strange type. It would seem to be a function that takes no arguments, and returns a value of type `IO ()`? It won't make complete sense until the next chapter, but for now, remember two things.

    1) `IO` is a type constructor which takes a type!marginnote(You should be familiar with other type constructors from the last chapter. They're things like `Maybe` or `Either`, they take one or more types and return a concrete type.). But `IO` is special and known by the Haskell runtime, and it's the magic that allows us to accomplish impure things, like reading and writing files or printing to the terminal. 

    2) `()` is the type of a tuple with nothing in it. The tuples we've seen before have all had types like `(Int, Bool)` or `(Int, Int, Int)`, but they can have one or even no values, like `(Int)` or `()`. The only tuple of type `()` is `()`!marginnote(Since the type `()` only has one value, it takes no memory at runtime.), which is sometimes pronounced "unit".

3) `main = do`: `main` is a special function that Haskell treats in a different way from all the rest. It's similar to `main` functions in other languages: it's the function Haskell calls for you, so it's where your program begins!marginnote(`main` has some other unique properties as well, relating to `IO`.). What we have on this line is the first line of a multi-line function. 
    
    `do` is special syntax that we won't cover until the next section, so don't worry about it just yet. In our case, the program would evaluate the exact same without it, so **just delete it for now**. This is the first change we're making to this project!

4) `putStrLn "hello world"`: Like `print`, `printf`, or `console.log` in other languages, `putStrLn` is a function that takes some text and writes it to the terminal (with a newline at the end). Now, how can a Haskell function, which is supposed to take some input and return a result based on that input and do nothing else, possibly write to the terminal! And what would it return? Let's examine the type of `putStrLn`.

    ```haskell
    Prelude> :t putStrLn
    putStrLn :: String -> IO ()
    ```

    So it takes a value of type String and returns a value of type `IO ()`? Well that makes some sense to the typechecker, since `main`'s type is `IO ()`. But what does all this `IO ()` business actually mean? We will explain that very soon, but first let's actually run the project.

## Running the Project

!newthought(Now, we) need to run this! A very easy way to test our work is to load it in GHCi. In the root directory of `project1`, run `stack ghci`. This is similar to running `ghci` on its own, but it's a special Stack command that will automatically load the project. 

Running `stack ghci`, you should see some output then eventually be greeted by a somewhat-familiar prompt.

```haskell
*Main>
```

Now, remember our confusing `main` function from before? It takes no parameters, so we can just type `main` to run it.

```haskell
*Main> main
hello world
```

This is how we run our program for testing. Note that this is the first time we've seen something like this! if our main function was just defined as the string `"hello world"`, we'd see quotes around it.

```haskell
*Main> main' = "hello world"
*Main> main'
"hello world"
```

We can also just write `putStrLn "hello world"` to print `hello world`, instead of calling `main`!marginnote(Remember, `putStrLn` takes a string, prints it to the terminal, then returns an `IO ()`.).

```haskell
*Main> putStrLn "hello world"
hello world
```

Let's change our function from printing a boring `hello world` to a more modern `Hello, World!`. Just go into `src/Main.hs` and change 

```haskell
main =
  putStrLn "hello world"
```

to 

```haskell
main =
  putStrLn "Hello, World!"
```

Then go back to GHCi and type `:r`. Short for `:reload`, this reloads everything we've loaded into GHCi. 

```haskell
*Main> :r
[1 of 1] Compiling Main ( ...\project1\src\Main.hs, interpreted )
Ok, one module loaded.
*Main> main
Hello, World!
```

Now, it's time to explain what all this strange `IO ()` business represents.

## Containing Impurity

!newthought(This section) provides background for the next but is not required for understanding anything in this book. However it's quite interesting!

Haskell has some important principles. First, purity, which includes *referential transparency*. It means if you call a function, and the function returns `3`, that's the same as if you just wrote `3` instead of calling the function with those parameters. **Functions in Haskell must always return the same value for the same parameters**. The other restriction, which is closely related, is a lack of side effects. This means functions can't do anything besides return a result. They can't write a file, open a browser, draw to the screen, etc. This provides Haskell with an issue: how is it supposed to do anything productive if functions can't do anything besides return values? We couldn't print to the terminal, draw on the screen, or do anything interesting.

This conundrum is resolved with two things: the `main` function, and the `IO` type constructor. 

`main` is a special Haskell function. Your `main` function will be automatically executed by Haskell, and it should be of type `IO ()`. Haskell will evaluate this function, and execute all the *IO actions* returned by `main`. An example of an IO action is the output of `putStrLn "hello"`. It contains some instructions to Haskell telling it to print `hello`. `IO` and `main` are *magic* in Haskell. That means they're special constructs that let you do things that would be impossible otherwise!marginnote(On the other hand, `do` is not magic. It's just special syntax, which is very helpful but we won't be using it just yet. `IO` is somewhat complicated, and it's easier if you don't have strange magic-seeming syntax confusing you.)

!newthought(Here's why) we need `IO`. Let's say we had a function that was very un-Haskell-like. Imagine a hypothetical function `readFromFoo`. The first time you call it, it will open a file `Foo.txt` and return the first character. The second time, it will return the second character. The third time, it will return the third character, etc. `readFromFoo` is **not** a real function, so this won't work, but here's how it might look in GHCi.

!marginfigure(`readFromFoo` is impure - it does not have the same output for the same input (in this case, no input).)(/static/wise_mans_haskell/assets/exp_impurity_demo.svg)

```haskell
Prelude> :t readFromFoo
readFromFoo :: Char
-- For example purposes, let's say `Foo.txt` contains `ABC123`.
Prelude> readFromFoo
'A'
Prelude> readFromFoo
'B'
Prelude> readFromFoo
'C'
Prelude> readFromFoo
'1'
Prelude> readFromFoo
'2'
Prelude> readFromFoo
'3'
```

This is an impure function. As you can see, every time we call it, its input stays the same (we don't pass it anything), but its output changes. This is bad because GHC assumes all functions are pure. This means that it will assume that if it evaluated `readFromFoo` once, it won't have to evaluate it again, and can just re-use the same result from last time. So our code might actually end up looking like this!

```haskell
Prelude> readFromFoo
'A'
Prelude> readFromFoo
'A'
Prelude> readFromFoo
'A'
[...]
```

This is obviously not what we intended!marginnote(The issue, really, is that GHC assumes functions are pure and does optimizations based on that assumption. If that assumption is not correct, then GHC's attempts at optimization can break our code! This section discusses how we can force GHC to not optimize our impure functions.). But that's okay, this is easily fixed. GHC will only try to reuse the old results if the parameters were the same. So we can just add a parameter that does nothing, just to keep GHC from re-using old results, and the output will be what we want.

```haskell
Prelude> readFromFoo 17281
'A'
Prelude> readFromFoo 387
'B'
Prelude> readFromFoo 2811002
'C'
```

I just made these numbers by typing randomly on my keyboard. But this seems kind of inelegant - having to spam your keyboard every time we want to read from our file. But there's an even bigger issue that this doesn't even fix! 

Haskell provides very few guarantees about the order you can expect evaluation to happen. We only know one function `a` will be called before function `b` if the result of `a` is a parameter to `b`!marginnote(This is called [dependency injection](https://www.wikiwand.com/en/Dependency_injection).). For example, in `foo (bar x)`, `bar` must be evaluated before `foo`, because an output of `bar` is an input of `foo`.   

That's not the case here, so we don't know what order evaluation will happen in! Consider the following function:

```haskell
readFromFooTwice = [first, second] 
                   where first = readFromFoo 2423
                         second = readFromFoo 414
```

The random numbers keep Haskell from re-using the value of `readFromFoo`. But, GHC would be free to evaluate the two `readFromFoo`s in any order it wanted! If we're lucky we could get `first` evaluated first, and `second` evaluated second, in which case `readFromFooTwice` would return `"AB"`!marginnote(Remember, calling `readFromFoo` 6 times is supposed to return `'A'`, `'B'`, `'C'`, `'1'`, `'2'`, `'3'`.). But they could just as easily be evaluated in the opposite order, leaving us with `"BA"`! To fix this, let's change what `readFromFoo` actually does. Now, instead of just returning a `Char`, it'll return a tuple of type `(Char, Int)`. The `Int` is just whatever `Int` we passed in, incremented by one.

```
Prelude> readFromFoo =
('A', 1)
Prelude> readFromFoo 387
('B', 388)
```

!marginfigure(The changes made to our `readFromFoo` function)(/static/wise_mans_haskell/assets/exp_readFromFoo_comparison_1.svg)

Then, instead of just passing in random values, we'll pass the result of the first as an input to the second! Let's rewrite our fake-function `readFromFooTwice`.

```haskell
readFromFooTwice = [first, second] where
                      (first, int1) = readFromFoo 2423
                      (second, int2) = readFromFoo int1
```

Now, GHC will have to evaluate the `readFromFoo 2423` before it can evaluate `readFromFoo int1`, because it doesn't know what `int1` will be yet! You'd think the problem is solved, this is really only a band-aid. Let's say we wanted to call `readFromFooTwice` twice. 

```haskell
readFromFooFourTimes = oneAndTwo ++ threeAndFour where
                      oneAndTwo = readFromFooTwice
                      threeAndFour = readFromFooTwice
```

Just like before, GHC could run `oneAndTwo` after it ran `threeAndFour`! There's nothing that compels it to run `oneAndTwo` first. And worse, because we do `readFromFoo 2423` in `readFromFooTwice`, there's nothing that stops Haskell from reusing the output, even though that isn't what we want. What we wanted was:

```haskell
Prelude> readFromFooFourTimes
"abc1"
````

But we might end up with

```haskell
Prelude> readFromFooFourTimes
"c1ab" -- or worse, "abab"!
```

Do you remember how we modified `readFromFoo` to return the number that was passed to it, incremented by one? We can solve the issue with `readFromFooTwice` the same way. Let's modify `readFromFooTwice` to take an `Int`, and return a tuple with the result and the extra value.

```haskell
readFromFooTwice int0 = ([first, second], int2) where -- notice the new parameter, int0
                      (first, int1) = readFromFoo int0 -- we use the new parameter instead of a hardcoded number.
                      (second, int2) = readFromFoo int1
```

Now, we can modify `readFromFooTwice` so it takes a number and uses that instead of using the name number every time. Let's rewrite `readFromFooFourTimes`.

```haskell
readFromFooFourTimes int0 = (oneAndTwo ++ threeAndFour, int1) where
                      (oneAndTwo, int1) = readFromFooTwice int0
                      (threeAndFour, int2) = readFromFooTwice int1
```

Let's look at the flow of `Int`s here. `readFromFooFourTimes` takes `int0` and passes it to the first `readFromFooTwice`. Internally it goes into `readFromFoo`, comes out, goes into the next `readFromFoo`, and comes out of our `readFromFooTwice` function as `int1`. We then pass it to the second `readFromTwice`, where the process repeats.

You'll notice that, if we chose this style to represent our impurity, every function that touches impurity would have to return its result decorated with an extra `Int`. Any function that calls an impure function is "corrupted" by their impurity, and must do the dance of passing `Int`s around. Without too much effort, we've forced Haskell to never evaluate our functions out of order and never re-use previous values. Of course, we don't have to do all this when writing real Haskell. Haskell has a way to handle this all for us, and we don't even need to be aware it's doing it.

## The Sequencing Operator

!newthought(You can) think of an IO action, like the result of `putStrLn "hello world"`, as a piece of data that contains all the information necessary to execute that action. **Whatever that action would return, that's the type of `a` in `IO a`**. That's why `putStrLn` returns `IO ()`. It doesn't really make sense for it to return anything, so `()` is used to signify that - there's only one possible value of type `()`, so a function that returns type `()` means it doesn't tell you any new information.

Whatever `IO` action gets returned by `main`, Haskell takes it and executes it. This is another thing that makes `main` special!marginnote(Also, remember how we could write a value in GHCi and it would be evaluated, then the result would be printed? Well, there's an exception to that. If it's an `IO a` function, GHCi will execute the IO action inside. If one of those actions is writing to the terminal (as is the case of `putStrLn`), that's just what GHCi will do. Then, it'll look at the type `a`, the return type of the IO action we just performed. If it's not `()`, GHCi will print the return value. You'll see that when we introduce an `IO` function which isn't `IO ()`.).

Let's write a simple `IO ()` function. All it will do is write "helloworld" to the terminal.

```haskell
Prelude> helloWorld = putStrLn "helloworld"
Prelude> helloWorld
helloworld
Prelude> helloWorld
helloworld
```

You can think of `putStrLn "helloworld"` as returning a note that says "Print `helloworld` to the terminal". When GHCi sees the note, it follows the instruction and prints `helloworld`. Now, you can't run `show` on an `IO` value, so we can't really see inside. But, like our `readFromFoo` example above, The tricky part is you can't just throw `IO` values around, they have to be called from another `IO` function. To perform multiple IO actions in one function, we chain them together with the `>>` function. It returns a new IO action with the type of the last action in the chain.

Here's how we'd chain up two `putStrLn "helloworld"`s.

```haskell
Prelude> helloWorldDouble = putStrLn "helloworld1" >> putStrLn "helloworld2"
Prelude> helloWorldDouble
helloworld1
helloworld2
```

Let's try it some more.

```haskell
-- Let's also examine the type of (putStrLn "helloworld1" >>)
Prelude> :t (putStrLn "helloworld2" >>)
(putStrLn "helloworld2" >>) :: IO b -> IO b

-- So (putStrLn "helloworld2" >>) is a function that 
-- takes a value of type IO b and returns an IO b. 
-- Whatever (putStrLn "helloworld2") returns is essentially thrown away. 

-- Let's see what the type is when we provide it with a second argument.
Prelude> a = putStrLn "helloworld2" >> putStrLn "helloworld3"
Prelude> :t a
a :: IO ()

-- So putStrLn "helloworld2" >> putStrLn "helloworld3" returns something of type IO (). 
-- That means we can pass it to >> again, like this
Prelude> helloWorldTriple = putStrLn "helloworld1" >> (putStrLn "helloworld2" >> putStrLn "helloworld3")
Prelude> helloWorldTriple
helloworld1
helloworld2
helloworld3
```

Like all Haskell functions, `putStrLn "helloworld1"` is totally pure. We just cheat because `IO` types contain some instructions to do possibly-impure things, and GHCi executes those instructions when it sees them. 

If you didn't understand everything that went on here, that's okay. Just remember: **we can chain `IO a` actions together with `>>`, and `main` is a function that's automatically called so your program starts**.

## The Bind Operator

!newthought(Now, that's) well and good, but what if we want to do some IO action based on the value of another IO action? To explain this we'll be using lambdas, so let's refresh our memory of how they work.

```haskell
-- Create an anonymous function that takes a value, then applies show to it, then applies putStrLn to that, then returns the result.
Prelude> myLambda = (\x -> putStrLn . show $ x)

Prelude> myLambda 3
3
Prelude> myLambda "hello"
"hello"

-- Since we call show on the input, the type of whatever we pass in must be a member of the Show typeclass
-- putStrLn's return type is IO (), and since we return putStrLn . show $ x, our lambda's return type is also IO ().
Prelude> :t myLambda
myLambda :: Show a => a -> IO ()
```

Now that you've seen `>>`, let's talk about the `>>=` function, pronounced "bind". Anything you can do with `>>` you can also do with `>>=`, so `>>=` is a bit more popular. Remember how `>>` worked?

```haskell
Prelude> helloWorldDouble = putStrLn "helloworld1" >> putStrLn "helloworld2"
Prelude> helloWorldDouble
helloworld1
helloworld2
```

It took two values, in our case `IO ()` and `IO ()`, and chained them together!marginnote(In general though, they wouldn't both have to be `IO ()`.). Remember, the `a` in type `IO a` is the return value for the IO action we're performing. Take, for example, the `getLine` function. Here's the type for `getLine`.

```haskell
Prelude> :t getLine
getLine :: IO String
```

Go into GHCi and play with this. Can you figure it out? Don't forget that if you give GHCi a value of type `IO a`, GHCi will execute it and then if `a` is not `()`, it'll call `show` on it and print the result.

Did you try it?

```haskell
Prelude> getLine
test
"test"
Prelude> getLine
Woah, Haskell is cool!
"Woah, Haskell is cool!"
```

Here's the answer. `getLine` is an IO function, of course. What it does is prompt the user to enter some characters, then return that those characters inside an `IO String` type. This makes sense: whatever the `a` is in `IO a` is the type of the result of the `IO` action, and `String` is the natural result type for a function that lets the user input some text into the terminal. 

Now, let's see the `>>=` operator. Let's redefine `helloWorldDouble` to use `>>=` instead of `>>`.

```haskell
Prelude> helloWorldDouble = putStrLn "helloworld1" >>= (\_ -> putStrLn "helloworld2")
Prelude> helloWorldDouble
helloworld1
helloworld2
```

Instead of passing `>>=` a plain `putStrLn "helloworld2"`, we wrapped it up in a lambda that takes one parameter and does nothing it. The reason we did this is that `>>=` has to take a function as it's right-hand argument. What it's actually doing here is taking the output of `putStrLn "helloworld1"` (which is `()`) and passing it to the `(\x -> putStrLn "helloworld2")` function, which ignores it and returns an IO action that prints `helloworld2` to the terminal. However, that's not where `>>=` shines. It's most useful in situations where you actually care about the output of an IO action. Let's use it with `getLine` to have a little more fun.

```haskell
-- Let's use a simple function that takes a value and prints it with two exclamation marks after it. Then we'll use that function as the argument to(getLine >>=).
Prelude> beLoud = getLine >>= (\x -> putStrLn (x ++ "!!"))
Prelude> beLoud
I like Haskell
I like Haskell!!
```

You really should play with this, as having a firm understanding of what's going on here is critical. It's running the `getLine` function, which returns an `IO String`. Then `>>=` extracts the `String` and passes it to the function `\x -> putStrLn (x ++ "!!")`. That function takes the string, then returns an IO action that prints it with `"!!"` added to the end. Notice that even though `getLine` returns an `IO String`, the `x` is just a regular `String`. The bind operator "unwraps" it.

The powerful thing about the `>>=` operator is that you can chain it as much as you want. Let's use it to write a function that uses five different IO actions.

```haskell
Prelude> spongebob = putStrLn "Are you ready, kids?" >>= (\_ -> (getLine >>= (\_ -> (putStrLn "I can't hear you!" >>= (\_ -> (getLine >>= (\_ -> putStrLn "Ohhhhh!")))))))
Prelude> spongebob
Are you ready, kids?
Aye aye, captain
I can't hear you!
Aye aye, captain!
Ohhhhh!
```

Now, you might have thought that was somewhat of a pain to type into GHCi!marginnote(You *are* typing all these examples into GHCi, right?). Well luckily for us, Haskell provides something called `do` notation to save us some work.

## Do Notation

!newthought(Let's look) at that spongebob example again, but this time let's put it in a file.

```haskell
-- spongebob.hs
!include(haskelltests/should_compile/spongebob.hs)
```

This is, to put it mildly, a mess. Let's add some newlines to make it slightly more clear what's going on.

```haskell
-- spongebobNewlines.hs
!include(haskelltests/should_compile/spongebobNewlines.hs)
```

That's a bit more readable. But we're still typing a lot of repetitive lambdas. Do you know what type `spongebob` will return?

```haskell
Prelude> :t spongebob
spongebob :: IO ()
```

Whatever is the type of the last element in our huge chain, that's what type the whole thing will return. Since `putStrLn "Ohhhhh!"` returns type `IO ()`, and it's the last thing that'll get done in our sequence, that's the type of the `spongebob` function.

But although this formatting is better, we're forced to write `<stuff> >>= \_ -> <stuff>` over and over, which is a bit annoying when we'd rather just be focusing on the logic of our program. That's why Haskell provides `do` notation. Let's see how it works

```haskell
-- spongebobDo.hs
!include(haskelltests/should_compile/spongebobDo.hs)
```

Look how simple that made it! Remember, this is just syntax sugar. Before this is compiled, `do` notation is converted to use `>>=`, so Haskell doesn't know or care which you use!marginnote(Lots of beginners think `do` is magic that lets you use `IO` functions, but it's not. And it actually works on much more than just IO! We'll cover that a bit later.). Anything you can do with `do` you can do with `>>=`.

```haskell
spongebob =
    putStrLn "Are you ready, kids?" >>= \_ ->
    getLine                         >>= \_ ->
    putStrLn "I can't hear you!"    >>= \_ ->
    getLine                         >>= \_ ->
    putStrLn "Ohhhhh!"
```

**`do` does not do anything magic, it's just shorthand for long chains of `>>=`**. However, our program is kind of boring, because we never use the results of previous operations in our next ones. This is how we might do that, using `>>=`.

```haskell
-- spongebobResults.hs
!include(haskelltests/should_compile/spongebobResults.hs)
```

Let's try it out!

```haskell
Prelude> :l spongebobResults.hs
[1 of 1] Compiling Main ( spongebobResults.hs, interpreted )
Ok, one module loaded.
*Main> spongebob
Are you ready, kids?
aye aye, captain
I can't hear you!
aye aye, captain!
Ohhhhh!
Results: You said 'aye aye, captain' the first time, and 'aye aye, captain!' the second time
```


To do this using `do`, there's an extra piece of notation we need. It looks like a backwards arrow: `<-`!marginnote(It's totally unrelated to the forward arrow in a lambda (`\x ->`).).git c

```haskell
--- spongebobResultsDo.hs
!include(haskelltests/should_compile/spongebobResultsDo.hs)
```

This is cool! With `first <-` and `second <-`, when Haskell is converting our `do` syntax to regular `>>= \_ ->` syntax, it knows to change the `_` to `first` or `second`, depending. For clarity, we could just always use `<-`, like this.

```haskell
spongebob = do
    _      <- putStrLn "Are you ready, kids?"
    first  <- getLine
    _      <- putStrLn "I can't hear you!"
    second <- getLine
    _      <- putStrLn "Ohhhhh!"
    putStrLn ("Results: You said '" ++ first ++ "' the first time, and '" ++ second ++ "' the second time")
```

We can't bind the last one to anything, for two reasons. For one, there would be no point, it's at the end so there would be nowhere to use it!marginnote(when you make a binding in a `do` expression, it can only be used later in that expression.). And for two, scroll up and look at `spongebobResults.hs`. There's no lambda that comes after the last `putStrLn`, so there's nowhere to bind the value!marginnote(It's edge cases like these when it comes in handy to know that `do` is just a prettier version of `>>=`.).

Note that when using do notation, if the first element is type `IO a`, every other element has to be some other `IO` type. This would not be allowed:

```haskell
-- badDo.hs
!include(haskelltests/error_examples/badDo.hs)
```

`(1 + 1) :: Int` is, obviously, an `Int`. And since the first thing that was encountered in the `do` expression was `putStrLn "hello"` (which is an `IO` type), it wants everything that follows to be an `IO` type!marginnote(Yes, that implies `do` works with things other than `IO` - it does! We'll cover that later.).

If you want to bind `(1 + 1)` to a name inside a `do` expression, you need to turn it into an `IO` action. For that, we have the `pure` function, which takes a value and makes it an `IO` type. Observe:

```haskell
-- goodDo.hs
!include(haskelltests/should_compile/goodDo.hs)
```

The `pure` function takes a value, in our case an `Int`, then puts it into an `IO` value. So it turns an `Int` into an `IO Int`. Then the `<-` extracts the `Int` out of the `IO Int`, leaving `two` equal to `2`!marginnote(There's another function, `return`, which does the same thing as `pure`. `pure` will work in cases `return` won't, so `pure` is recommended.). This may seem convoluted, but that's how it's got to be done. In practice it doesn't come up that much because you can always use `let` and `where`:

```haskell
-- goodDo2.hs
!include(haskelltests/should_compile/goodDo2.hs)
```

Now that you understand how to effectively use `IO` functions, you need to learn what they are.

## Useful IO Functions

1) `putStrLn :: String -> IO ()`

    This function takes a `String` and returns an IO action that will print it to the terminal, with a newline at the end.

2) `putStr :: String -> IO ()`

    This function takes a `String` and returns an IO action that will print it to the terminal, without a newline at the end.

3) `putChar :: Char -> IO ()`

    Like `putStr`, but takes a single character instead of a string.

4) `print :: Show a => a -> IO ()`

    Equivalent to `putStrLn . show`. Takes a value `x` and returns an IO action that'll print `show x`.

5) `getLine :: IO String`

    Returns an IO action that will allow the user to type some text into the terminal, then returns that text.

6) `openFile :: FilePath -> IOMode -> IO Handle`

    This is part of the `System.IO` package, which means that in order to use it, you have to write `import System.IO` at the top of every file that uses it. What it does is take a path to a file (which is just a `String`) and an `IOMode` (which is either `ReadMode`, `WriteMode`, `AppendMode`, or `ReadWriteMode`), and return a "file handle" which is basically a reference to a file. If you opened the file in `ReadMode` or `ReadWriteMode` you can get the contents with the `hGetContents` function. When you're done with the file, run `hClose` on the handle to tell the computer you don't need it anymore. Let's how to read a file.

    ```haskell
    import System.IO  
  
    main = do  
        handle <- openFile "myFile.txt" ReadMode  
        contents <- hGetContents handle  
        putStr contents  
        hClose handle  
    ```

    Writing a file is more complicated due to Haskell's laziness, so we recommend using either the [Pipes](http://hackage.haskell.org/package/pipes) or [Conduit](http://hackage.haskell.org/package/conduit) library for it. We'll discuss how to use Pipes in a later chapter.

7) `sequence`

    This is an interesting function!marginnote(We won't display `sequence`'s type signature because it's a little complicated/), what it does is take a list of `IO` actions and return an IO action that executes all of them.
    
    ```haskell
    Prelude> sequence [print "hello", print 3, putStrLn "bonjour"]
    "hello"
    3
    bonjour
    [(),(),()]
    ```

    Note that it's return type is not `IO ()`, it's `IO [a]`, so GHCi will attempt to print its output. That's what `[(),(),()]` is at the bottom. It's output is the output of all the IO actions in the list.

## Project 1 - A Text Adventure Game

!newthought(This is) a project we're going to use to test some of the skills we've learned so far! We're going to make a type of game called a text adventure game. Here's how it'll look when we're finished:

```haskell
You awaken in a dark cavern, only the dimmest light illuminating your surroundings.
You try to remember how you got there, but you realize you can't remember anything - not even your own name.
You look around, and see two pinpricks of light in the distance, one to your left and one to your right.
Could they be exits?
  1: Probably not, better to sit here and wait for someone to find me.
  2: Hmm... I'll go explore the possible exit on my right.
  3: I have a good feeling about the light to my left
Choice: 1
Story over!
```

The first thing to do is to make a new project - let's call it `textAdventure`, but you're free to call it whatever you like. This is the command you'd run to start a project!marginnote(you may want to change the name, of course.):

```haskell
stack new textAdventure simple -p "author-name:Andre Popovitch"
```

Then, navigate into the `textAdventure` directory and run `stack ghci` to open an environment where we can run our program. Also, open a text editor and view `src/Main.hs` - this is the only file we need for doing our work!marginnote(In general, you'll spend most of your time editing files in the `src` directory.). 

The first thing to do is think about what we want. We need to be able to show some text, like our `You awaken in a dark cavern ...`. Let's make a data type that will hold a piece of text to display to the user, we'll call it `Message` because it's a message to the user. If the text is to represent someone talking, we'll store his or her name separately from what they say. Add this to `Main.hs`

```haskell
data Message = Speaking {texts :: [String], speaker :: String}  | Info {texts :: [String]}  
```

We're using record syntax here - reread [Creating New Data Types](../creating-new-data-types) if you've forgotten how this works. 

We also want to be able to display choices to the user. Let's make a data type to store a single choice, and a data type to store our whole adventure.

```haskell
data Choice = Choice {choice :: String, result :: Adventure}
data Adventure = StoryEnd | StoryMessage Message Adventure | StoryChoice [Choice]
```

You might notice that `Choice` can contain an `Adventure` and an `Adventure` can contain a `Choice`. This makes it a *recursive data type*. Nesting the data types like this will allow us to have an Adventure as long as we want.

!newthought(Let's write) an `IO` function, `tellStory`, which will take an `Adventure` and recursively!marginnote(Recursive data types often demand recursive functions.) present it to to the user.

```haskell
tellAdventure :: Adventure -> IO ()
```

Let's think of the three cases our `tellStory` function will have to handle. Whatever `Adventure` we pass it, it's either going to be a `StoryEnd`, a `StoryMessage Message Adventure`, or a `StoryChoice [Choice]`. If it's a `StoryEnd`, that's it for our story, so we can just write that the story is over and be done.

```haskell
tellAdventure StoryEnd = 
  putStrLn "Story over!"
```

Now, if the `Adventure` our function gets is a `StoryMessage Message Adventure`, we want to print out the messages, and then continue on with the nested `Adventure`.

```haskell
tellAdventure (StoryMessage message adventure) = do
  sequence . map putStrLn . texts $ message 
  tellAdventure adventure
```

This is pretty simple. The text gets extracted from the message with `texts`, yielding a list of `String`s. `map putStrLn` converts all of those `String`s to IO Actions that print that string. `sequence` converts that list of IO Actions to a single IO action which will execute every action in the list. The end result is that every `String` inside `message` gets printed to the terminal. Then, we call `tellAdventure` on the `adventure` inside the `StoryMessage` to continue the story.

The last thing we need is to do is to show the users a List of choices, allow them to choose one, and then continue with the adventure that's contained within that choice. We'll do this by numbering the choices, use `getLine` to let the user enter a number, and continue with the adventure in the corresponding `Choice`!marginnote(Remember, each `Choice` contains an `Adventure`.).

```haskell
tellAdventure (StoryChoice choices) = do
  sequence . map putStrLn . map choiceToString $ indexedChoices
  putStr "Choice: "
  userChoiceIndex <- getLine
  tellAdventure . result . (choices !!) . (\x -> x-1) . read $ userChoiceIndex
  where indexedChoices = zip [1..] choices
        choiceToString (i, c) = "  " ++ (show i) ++ ": " ++ (choice c)
```

We use `zip` to get a list of tuples of choices and their respective number. Then we convert the choices to strings, which is just some simple string manipulation in the `choiceToString` function. We then print all those strings using `sequence . map putStrLn` like we did in the last function.

Next, we run `userChoiceIndex <- getLine` which allows the user to type in the terminal, and we bind whatever they typed to the name `userChoiceIndex`.

The last part of the function is the line: 

```haskell
tellAdventure . result . (choices !!) . (\x -> x-1) . read $ userChoiceIndex
```

Which turns `userChoiceIndex` into a number with `read`, subtracts `1` from it, gets the `choice` at that index with `!!`, gets the `Adventure` from that choice with `result`, then calls `tellAdventure`. Easy as pie!  

Here's the whole `tellAdventure` function, for those following along:

```haskell
tellAdventure :: Adventure -> IO ()
tellAdventure StoryEnd = 
  putStrLn "Story over!"
  
tellAdventure (StoryMessage message adventure) = do
  sequence . map putStrLn . texts $ message
  tellAdventure adventure

tellAdventure (StoryChoice choices) = do
  sequence . map putStrLn . map choiceToString $ indexedChoices
  putStr "Choice: "
  userChoiceIndex <- getLine
  tellAdventure . result . (choices !!) . (\x -> x-1) . read $ userChoiceIndex
  where indexedChoices = zip [1..] choices
        choiceToString (i, c) = "  " ++ (show i) ++ ": " ++ (choice c)
```

Now, we just need to write the `Main` function and put it all together.

```haskell
main :: IO ()
main = do
  tellAdventure story
  where story = StoryMessage (Info ["You awaken in a dark cavern, only the dimmest light illuminating your surroundings.", 
                                    "You try to remember how you got there, but you realize you can't remember anything - not even your own name.",
                                    "You look around, and see two pinpricks of light in the distance, one to your left and one to your right.",
                                    "Could they be exits?"]) 
                             (StoryChoice [Choice "Probably not, better to sit here and wait for someone to find me." StoryEnd, 
                                           Choice "Hmm... I'll go explore the possible exit on my right." StoryEnd, 
                                           Choice "I have a good feeling about the light to my left" StoryEnd]) 
```

We define `story`, and then call `tellAdventure` on it. If you try and extend the definition of `story`, be careful to mind your indentation! Haskell can be picky!marginnote(Fortunately, this pickiness tends to make sure your programs are aesthetically pleasing.). This story is very simple - it displays a message, then offers 3 choices, all of which end the story. If you want, you can change `StoryEnd` to some other value of type `Adventure`, like a `StoryMessage` or another `StoryChoice`. 

## Exceptions

!newthought(Recall the) `head` function. It takes a List and withdraws the first element from that List. So `head [1,2,3]` will return `1`. But what happens if the List is empty? Open GHCi and try to use `head []` to get the first value of an empty List.

```haskell
Prelude> head []
*** Exception: Prelude.head: empty List
```

This makes some sense - There's no first element of an empty List, so Haskell has no idea what to do. Instead, it *throw an exception*. If you don't take care, a thrown exception can crash your whole program. Since `head` works properly with some inputs and creates an exception with others, it's called a *partial function*. Functions that never throw an exception, no matter the input, are called *total functions*.

One way of handling partial functions is to just structure our program so they will never be provided with an input that results in a crash. For example, if you have a function that you know will always return a List with some number of elements, it's safe to call `head` on the result of that function. However, this is sometimes tricky. If your function has a bug and accidentally returns an empty List, Haskell's type system will not warn you and your program will crash at runtime!marginnote(A goal of Haskell is to catch as many bugs at compile time as possible.)! Let's see another approach.

```haskell
-- headMaybe.hs
!include(haskelltests/should_compile/headMaybe.hs)
```

So what does this do? If you try to pass `headMaybe` an empty List, it will return `Nothing`. But if you pass it any List that is not empty, it will return `Just <first element of the List>`.!marginnote(If you're confused by `Maybe`, re-read the section on [parameterized types](Parameterized Types)). So this function is total even though it uses the partial function `head`, because it only uses `head` in situations where it has no chance of throwing an exception. Let's see it in action.

```haskell
Prelude> :l headMaybe.hs
[1 of 1] Compiling Main ( headMaybe.hs, interpreted )
Ok, one module loaded.
*Main> headMaybe []
Nothing
*Main> headMaybe [1,2,3]
Just 1
```

Looks good to me. Do you remember our text adventure project? It had a small issue in it. If the user entered something other than a number when asked for their choice, when we call `read` on that value the program will crash because `read` can't convert it. Luckily for us, there's another useful function inside the `Data.Text` module called `readMaybe`. `readMaybe` is the same as read, except wrapped up in a `Maybe` so it can return `Nothing` if the value couldn't be converted.!marginnote(If you remember, a module is a collection of related code. Not all of this code is made available at the start of your program due to *name collisions*, a concept that will be explained shortly.)

To use a module, you have to *import* it with the `import` keyword at the start of your program. Here's a demonstration. If you recall, in the last section we made this file:

```haskell
module Main where

data Message = Speaking {texts :: [String], speaker :: String}  | Info {texts :: [String]}  

data Choice = Choice {choice :: String, result :: Adventure}

data Adventure = StoryEnd | StoryMessage Message Adventure | StoryChoice [Choice]

tellAdventure :: Adventure -> IO ()
tellAdventure StoryEnd = 
  putStrLn "Story over!"
  
tellAdventure (StoryMessage message adventure) = do
  sequence . map putStrLn . texts $ message
  tellAdventure adventure

tellAdventure (StoryChoice choices) = do
  sequence . map putStrLn . map choiceToString $ indexedChoices
  putStr "Choice: "
  userChoiceIndex <- getLine
  tellAdventure . result . (choices !!) . (\x -> x-1) . read $ userChoiceIndex
  where indexedChoices = zip [1..] choices
        choiceToString (i, c) = "  " ++ (show i) ++ ": " ++ (choice c)
  

main :: IO ()
main = do
  tellAdventure story
  where story = StoryMessage (Info ["You awaken in a dark cavern, only the dimmest light illuminating your surroundings.", 
                                    "You try to remember how you got there, but you realize you can't remember anything - not even your own name.",
                                    "You look around, and see two pinpricks of light in the distance, one to your left and one to your right.",
                                    "Could they be exits?"]) 
                             (StoryChoice [Choice "Probably not, better to sit here and wait for someone to find me." StoryEnd, 
                                           Choice "Hmm... I'll go explore the possible exit on my right." StoryEnd, 
                                           Choice "I have a good feeling about light to my left" StoryEnd]) 
```

Look at the first line:

```haskell
module Main where
```

This says we're creating a new module named `Main`. To import a module, we add an import statement beneath:

```haskell
module Main where
import Text.Read
```

This will let us use all the functions in the `Text.Read` module!marginnote(If you import a module in your code, then import your code into GHCi, you automatically get access to everything that module imports.). But if we do this and try to compile the project with `stack ghci`, we will get the following error:

```haskell
    Ambiguous occurrence `choice'                                                                                                                                         
    It could refer to either `Text.Read.choice',                                                                                                                          
                             imported from `Text.Read' at [...]\textAdventure\src\Main.hs:2:1-16
                             (and originally defined in `Text.ParserCombinators.ReadPrec')                                                                                
                          or `Main.choice',                                                                                                                               
                             defined at [...]\textAdventure\src\Main.hs:6:23                    
   |                                                                                                                                                                      
19 |   sequence . map putStrLn . map (\x -> "  " ++ (show . fst $ x) ++ ": " ++ (choice . snd $ x)) $ indexedChoices                                                      
   |                                                                             ^^^^^^                                                                                   
```

Can you figure out the issue? `Text.Read` has a function called `choice`, and we have a function called `choice`, and Haskell doesn't know which to use! You can't have two functions with the same name, because that's confusing to you and confusing to the compiler. What we need to do is not import the `choice` function from `Text.Read`!marginnote(We could also rename our `choice` function, but that's annoying to have to do.). We can do that with the `hiding` keyword, like this:

```haskell
module Main where
import Text.Read hiding (choice)
```

This will tell Haskell to not import the `choice` function from `Text.Read`. Since we didn't import it, Haskell won't be tripped up by the value named `choice` we use later. But this isn't ideal. We only want the `readMaybe` function, not all the functions except `choice`. We want to only import what we need!marginnote(This is so we won't accidentally bump into a name collision again. Importing only what you need is usually a good idea.). Here's the syntax for that.

```haskell
module Main where
import Text.Read (readMaybe)
```

If we wanted to import multiple functions, we can just separate them by commas, like `(function1, function2, etc)`. Here's the type of `readMaybe`.

```haskell
Prelude> :t readMaybe
readMaybe :: Read a => String -> Maybe a
```

!newthought(Let's write) a function which will ask the user for a choice, and return an `(Integral a, Read a) => IO a` with the type the user entered. It needs to be a `Read` type because this function only makes sense for types that can be read from `String`s. It needs to be `Integral` because it wouldn't make sense for the user to enter, say `3.5`. `Integral` is a typeclass for numbers with nothing after the decimal point.  

```haskell
getChoice :: (Integral a, Read a) => IO a
getChoice = do
    putStr "Choice: "
    userChoiceIndex <- getLine
    maybeIndex <- pure . readMaybeInt $ userChoiceIndex 
    case maybeIndex of Nothing -> getChoice
                       Just index -> pure index 
  where
    readMaybeInt :: (Integral a, Read a) => String -> Maybe a
    readMaybeInt = readMaybe
```

Here's how this function works. In the `where` block in the bottom, we define a function called `readMaybeInt`. It does the same thing as `readMaybe`, except we've added another type restriction on the output, specifically whatever it returns needs to be in the `Integral` typeclass as well as in the `Read` typeclass. `readMaybe` on it's own would happily work on `3`, but it would also work on `3.5` or `True` or all sorts of other things we don't want. 

Now, let's tackle the `do` expression. First we do `putStr "Choice: "`. This should hopefully be familiar at this point, it prints the text `Choice: ` to the terminal without a newline at the end.

Then, we do `userChoiceIndex <- getLine`. This lets the user enter some text, and binds that text to the name `userChoiceIndex`.

Then, we do: 

```haskell
maybeIndex <- pure . readMaybeInt $ userChoiceIndex
```

This calls `readMaybeInt` on `userChoiceIndex`. It then uses the `pure` function, which turns the `Maybe a` returned by `readMaybeInt` into an `IO Maybe a`. `pure` just takes a value of any type, and returns that value inside an `IO` type. In this case, its type signature is `return :: a -> IO a`. We do this because every line in our `do` block has to be an `IO` type, but `readMaybeInt` returns a `Maybe a`, so the `pure` makes it a `IO Maybe a`. However, we bind it to `maybeIndex` with `<-` which removes the `IO` wrapping, so `maybeIndex` is back to being a `Maybe a`. The `<-` basically undoes all the work done by `pure`!media(Luckily GHC is smart enough to simplify this and not actually do anything.).

The last part of our function is a `case` expression. In case you forgot, `case` expressions allow us to do pattern matching inside a function. If `maybeIndex` is `Nothing`, the `case` expression evaluates to a recursive call of `getChoice`, starting the whole function over. Otherwise, the value is `Just index`, and we use `return index` to wrap index into an `IO`. Remember, the last expression of a `do` expression is what the whole thing returns, so this function will return an `IO` type that wraps up the index the user entered. 

Put this function somewhere in `Main.hs` of `textAdventure`, and load it into GHCi with `stack ghci`. Lets test this function out:

```haskell
*Main> getChoice
Choice: 3.5
Choice: True
Choice: 3
3
```

You can see that this `getChoice` will continually prompt us for choices until we enter a valid number, and then will return that number. Let's modify `tellAdventure (StoryChoice choices)` to use `getChoice`. Here's how it is now:

```haskell
tellAdventure (StoryChoice choices) = do
  sequence . map putStrLn . map choiceToString $ indexedChoices
  putStr "Choice: "
  userChoiceIndex <- getLine
  tellAdventure . result . (choices !!) . (\x -> x-1) . read $ userChoiceIndex
  where indexedChoices = zip [1..] choices
        choiceToString (i, c) = "  " ++ (show i) ++ ": " ++ (choice c)
```

All we have to do is remove line 3, change `getLine` to `getChoice` on line 4, and remove `. read` on line 5.

```haskell
tellAdventure (StoryChoice choices) = do
  sequence . map putStrLn . map choiceToString $ indexedChoices
  userChoiceIndex <- getChoice
  tellAdventure . result . (choices !!) . (\x -> x-1) $ userChoiceIndex
  where indexedChoices = zip [1..] choices
        choiceToString (i, c) = "  " ++ (show i) ++ ": " ++ (choice c)
```

Type `:r`!marginnote(`:r` is short for `:reload`.) in GHCi to reload the project, and then type `main` to test it out! Now our program won't crash when you enter something that isn't a number. However, we still have one issue. You can enter any number, but if you enter `-5` or `23839` the program will crash. Let's modify `getChoice` so it takes a number which will serve as the maximum number it will allow. We'll also disallow numbers less than zero.

```haskell
getChoice :: (Integral a, Read a) => a -> IO a    -- Our function now takes an argument.
getChoice numberOfOptions = do
    putStr "Choice: "
    userChoiceIndex <- getLine
    maybeIndex <- pure . readMaybeInt $ userChoiceIndex
    case maybeIndex of Nothing -> getChoice numberOfOptions                   -- When calling getChoice recursively, we have to pass the input.
                       Just index -> if index <= numberOfOptions && index > 0 -- We check if index is inside the range of allowed numbers.
                                       then pure index                        -- If it is, return it.
                                       else getChoice numberOfOptions         -- Otherwise, recurse.
  where
    readMaybeInt :: (Integral a, Read a) => String -> Maybe a
    readMaybeInt = readMaybe
```

Then, we just modify the third line of the `tellAdventure` function that matches the `(StoryChoice choices)` pattern to pass the length of the List to `getChoice`.

```haskell
tellAdventure (StoryChoice choices) = do
  sequence . map putStrLn . map (\x -> "  " ++ (show . fst $ x) ++ ": " ++ (choice . snd $ x)) $ indexedChoices
  userChoiceIndex <- getChoice (length choices)                 -- call getChoice with the length of the List 
  tellAdventure . result . (choices !!) . (\x -> x-1) $ userChoiceIndex
  where indexedChoices = zip [1..] choices
```

And voilà! We have now removed the two biggest bugs in our program. But it's not very user friendly. If the user enters `one` when they meant `1`, we just show `Choice: ` again. It doesn't give any instruction for what the user should do. Let's make one final change to the `getChoice` function. Remember, `do` is just syntax sugar for an expression, so we can use it in the `where` block!marginnote(We can even use `do` expressions inside `do` expressions!).

```haskell
getChoice :: (Integral a, Read a, Show a) => a -> IO a   -- We need to call show on the input, so a needs to be a member of the show typeclass.
getChoice numberOfOptions = do
    putStr "Choice: "
    userChoiceIndex <- getLine
    maybeIndex <- pure . readMaybeInt $ userChoiceIndex 
    case maybeIndex of Nothing -> displayErrorAndRetry             -- Use the new displayErrorAndRetry function.
                       Just index -> if index <= numberOfOptions && index > 0
                                       then pure index
                                       else displayErrorAndRetry   -- Use the new displayErrorAndRetry function.
  where
    readMaybeInt :: (Integral a, Read a) => String -> Maybe a
    readMaybeInt = readMaybe
    displayErrorAndRetry = do             -- Define a function that prints an error then returns getChoice numberOfOptions.
      putStrLn $ "Please enter a number 1 through " ++ (show numberOfOptions) ++ "."
      getChoice numberOfOptions
```

Now if you reload GHCi with `:r` and then run `main`, you'll find a friendly error message when you enter an invalid value.

```haskell
*Main> main
You awaken in a dark cavern, only the dimmest light illuminating your surroundings.
You try to remember how you got there, but you realize you can't remember anything - not even your own name.
You look around, and see two pinpricks of light in the distance, one to your left and one to your right.
Could they be exits?
  1: Probably not, better to sit here and wait for someone to find me.
  2: Hmm... I'll go explore the possible exit on my right.
  3: I have a good feeling about light to my left
Choice: 5
Please enter a number 1 through 3.
Choice: 2
Story over!
```

There, that's very nice. You can have a `do` expression anywhere Haskell expects an expression!marginnote(Of course, your program must typecheck. The type of a `do` expression is the type of the last line.). That's one of the things that makes them so powerful. `do` expressions have lots of uses besides IO, but IO is what it was originally designed for, so I thought it was fitting to introduce them that way. If you've been following along, here's the whole project up to this point.

```haskell
!include(haskelltests/projects/textAdventure/src/Main.hs)
```


## Building Projects for Distribution

Often you'd like to send a `.exe` file (or similar) to someone, so they can enjoy and experience it themselves. To do that, we need to *build* our project. To do that, run `stack build` while in the `textAdventure/` directory. In the terminal, you should see some text that says `Installing executable textAdventure in <directory>`. Go to that directory, and you should see a nice little standalone file you can share with all your friends!

# More Helpful Typeclasses

## Functors

!newthought(In Haskell), whenever there's a container of some arbitrary "stuff" that can be mapped over, it's a good candidate for the `Functor` typeclass!marginnote(Something in the `Functor` typeclass is called a *functor*, *endofunctor*, or sometimes a *covariant endofunctor*. But you'd only ever call it anything but "functor" if you were trying to show off your category theory chops.). The most common example is Lists. There's only one function inside the `Functor` typeclass, and that's `fmap`, which is just like `map`.

```haskell
instance Functor [] where  
    fmap = map  
```

In general, `fmap` takes a function and a functor, and applies that function to the elements inside the functor. Let's refresh our memory of how it works on lists. 

```haskell
Prelude> fmap (+1) [1,2,3,4,5]
[2,3,4,5,6]
```

`Maybe` is also a functor. This is how it works:

```haskell
Prelude> fmap (+1) (Just 3)
Just 4
Prelude> fmap (+1) Nothing
Nothing
```

And here's how it's defined:

```haskell
instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing  
```

If you pass it a `Just <something>`, it will apply the function to the `<something>` inside. If you pass it `Nothing`, it will return `Nothing`. This is nice because it makes it easy for us to change the inside of a `Maybe`, but won't crash on us if the result is `Nothing`.

`fmap` also works on 2-tuples. What they do is surprising - they only operate on the second element of the tuple. Here's a demonstration.

```haskell
Prelude> fmap (+1) ("Hello", 4)
("Hello",5)
```

Functors have some rules, called *the functor laws*. There's a function called `id`, which is special because it does absolutely nothing. Here's how it's defined.

```haskell
id :: a -> a
id x = x 
```

`id` takes one argument, and returns that argument unchanged. The first functor law is that if you evaluate `fmap id <something>`, the result should be `<something>`. Whatever you pass to `fmap id` should be returned unchanged. 

The second functor law is that `fmap (f . g) = fmap f . fmap g`.  Applying `fmap g`, and then applying `fmap f`, should be the same as if you just applied `fmap (f . g)`. This is convenient because these two laws mean that `fmap` probably won't be doing anything other than just mapping over your function. Note that the functor laws aren't enforced by Haskell - it's up to the programmer to ensure they're true. If you make a data type an instance of `Functor`, you should make absolutely sure that it doesn't break any of the laws.


## Monoids

!newthought(A *monoid*) is very simple. It's a function (or operator) *f* which takes two arguments!marginnote(A function (or operator) which takes two parameters is also called a binary function.), and has a value that can be used as an *identity*, and is *associative*. We have two new words here, associative and identity. Let's discuss what they mean with three famous monoids, addition (`+`), multiplication (`*`), and string concatenation (`++`).

Let's look at addition. 

1) Does it take two arguments? Yes.

    ```haskell
    Prelude> 2 + 5
    7
    ```

2) Is it associative? That means parentheses don't matter when evaluating it!(To rearrange the parentheses in an expression is to *reassociate* it. The technical definition of "associative" is that you can reassociate any way you want without changing the meaning of the expression. ).

    ```haskell
    Prelude> 2 + 5 + 7 + 8
    22
    Prelude> (2 + 5) + (7 + 8)
    22
    Prelude> (2 + (5 + 7 + 8))
    22
    ```

    Seems that way!

3) Does it have an identity? For addition, that means "is there a value you can add to any number, and leave that number unchanged". There is, `0`.

    ```haskell
    Prelude> 16 + 0
    16
    ```

Let's look at multiplication.

1) Does it take two arguments? Yes.

    ```haskell
    Prelude> 2 * 5
    10
    ```

2) Is it associative? That means parentheses don't matter when evaluating it.

    ```haskell
    Prelude> 2 * 5 * 7 * 8
    560
    Prelude> (2 * 5) * (7 * 8)
    560
    Prelude> (2 * (5 * 7 * 8))
    560
    ```

    Seems that way!

3) Does it have an identity? For multiplication, that means "is there a value you can multiply by any number, and leave that number unchanged". There is, `1`.

    ```haskell
    Prelude> 16 * 1
    16
    ```

Now, let's look at string concatenation.

1) Does it take two arguments? Yes.

    ```haskell
    Prelude> "Hello, "  ++ "World!"
    "Hello, World!"
    ```

2) Is it associative? That means parentheses don't matter when evaluating it.

    ```haskell
    Prelude> "Good morning, " ++ "good afternoon, " ++ "good evening, " ++ "and good night!"
    "Good morning, good afternoon, good evening, and good night!"
    Prelude> ("Good morning, " ++  "good afternoon, ") ++ ("good evening, " ++ "and good night!")
    "Good morning, good afternoon, good evening, and good night!"
    Prelude> ("Good morning, " ++ ("good afternoon, " ++ "good evening, " ++ "and good night!"))
    "Good morning, good afternoon, good evening, and good night!"
    ```

    Seems that way!

3) Does it have an identity? For string concatenation that means "is there a value you can concatenate to any string, and leave that string unchanged. There is, `""`.

    ```haskell
    Prelude> "Monoids!" ++ ""
    "Monoids!"
    ```

Hopefully, this should give you an intuition for monoids. Since we're in a chapter about typeclasses, you might be confused. Being a monoid seems to be a property about operators, what does this have to do with types? Well, I mislead you somewhat. The correct phrasing would be to say that `String`s form a monoid under `++`. It's the type that's the monoid, not the operator. Of course, one type can be a monoid under multiple operations, but in Haskell, we usually single out one operation to be "the" operation for that monoid. For example, `String`s have `++`, so `String` is a monoid.

Here's how the `Monoid` typeclass is defined:

```haskell
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty
```

Let's look at what each of these means.

`mempty` is a function that returns the identity value for whatever monoid is in question. It's short for "monoid empty".

```haskell
Prelude> mempty :: String
""
Prelude> mempty ++ "hello"
"hello"
```

There's `mappend`, which takes two `Monoid`s and does whatever our monoid operation is on them.

```haskell
Prelude> "a" `mappend` "b"
"ab"
```

Lastly, there's `mconcat`, which takes a List of monoids and `mappend`s them all. 

```haskell
Prelude> mconcat ["1", "2", "3", "4"]
"1234"
```

You may have noticed there are two operations under which numbers are monoids, `+` and `*`. Because of this, most number types aren't in the `Monoid` typeclass, since they couldn't pick one operation to be *the* operation for numbers. It does, however, have the types `Product` and `Sum` in the `Data.Monoid` module.

```haskell
Prelude> import Data.Monoid
Prelude> Product 3
Product {getProduct = 3}
Prelude> (Product 3) `mappend` mempty
Product {getProduct = 3}
```

You may be wondering, what exactly is this `Product {getProduct = 3}` mess? It's because `Product` is a type which acts as a wrapper around a number, so it can be in different typeclasses. `getProduct` is the function we can use to get the actual number.

```haskell
Prelude> threeProduct = Product 3
Prelude> threeProduct
Product {getProduct = 3}
Prelude> getProduct threeProduct
3
```

Of course, there's also Sum, which works the same way except its monoidal operation is `+` instead of `*`, and you extract the original number with `getSum`.

```haskell
Prelude> a = Sum 3
Prelude> b = Sum 5
Prelude> a `mappend` b
Sum {getSum = 8}
Prelude> getSum a
3
```

2-tuples can also be monoids, provided they contain monoids. The contents on the inside are just `mappend`ed to each other.

```haskell
Prelude> ("Hello ", "Goodbye ") `mappend` ("world.", "planet.")
("Hello world.","Goodbye planet.")
```

Let's see how that works with the `Sum` an `Product` types.

```haskell
Prelude> import Data.Monoid
Prelude> (Sum 3, Product 4) `mappend` (Sum 10, Product 2)
(Sum {getSum = 13},Product {getProduct = 8})
```

So if you have two tuples of type `(a, b)`, and both `a` and `b` are `Monoid`s, then you can `mappend` the tuples and the contents will be `mappend`ed. Since regular numbers aren't monoids, this won't work.

```haskell
Prelude> (1,2) `mappend` (2,3)
-- error!
```

!newthought(Remember, if) you make one of your own types an instance of `Monoid`, it's important that you **make sure** that your implementation follows the monoid laws of associativity and identity. If not, other programmers and yourself can get very confused!

## Semigroups

!newthought(*Semigroups are) very simple - they're just monoids, but without the restriction that we must have an identity element. This means all monoids are semigroups. Any associative binary function forms a semigroup. Let's see an example with the `max` function. The `max` function takes two values and returns whichever is greater. 

```haskell
Prelude> max 3 5
5
Prelude> 3 `max` 5
5
```

Is it associative? Let's try.

```haskell
Prelude> 1 `max` 2 `max` 54 `max` 19 `max` (-1)
54
Prelude> 1 `max` (2 `max` (54 `max` 19) `max` (-1))
54
Prelude> (1 `max` 2 `max`) (54 `max` 19) `max` (-1)
54
```

Yes! A chain of `max`s will eventually output whichever value is highest, no matter where we put the parentheses. This means `max` is associative, so numbers form a semigroup under `max`. 

Semigroup isn't used very often, so let's move on. 


## Applicatives

!newthought(An *applicative*) is essentially a monoid mixed with a functor. Let me explain what that means.

As we've discussed before, Haskell functions are curried. This means that, while you can write `1+1` just fine, you can also just write `1+`, and then you get a function which will add one to whatever number you pass it. Let's play with that.

```haskell
Prelude> :t fmap (+) (Just 3)
fmap (+) (Just 3) :: Num a => Maybe (a -> a)
Prelude> :t fmap (+) [1,2,3,4,5,6]
fmap (+) [1,2,3,4,5,6] :: Num a => [a -> a]
```

`fmap (+) (Just 3)` returns a `Num a => Maybe (a -> a)`, a `Maybe` that contains a function. `fmap (+) [1,2,3,4,5,6]` returns a List of functions.

But we have an issue. If we have a `Just (1+)`, it'd be nice to have an easy way to apply that to the inside of a `Just 4` to get `Just 5`. That's where `Applicative` comes in.


```haskell
class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b  
```

You can see that for `f` to be an `Applicative`, it must first be a `Functor`. That's because `Applicative`s are just functors with extra features. It contains the `pure` function, which takes a value and wraps it up inside the applicative.

```haskell
Prelude> pure 4 :: Maybe Int
Just 4
Prelude> pure 4 :: [Int]
[4]
```

The intelligence of Haskell's type analysis allows it to select the correct `pure` function to call, based on the types it thinks we want. Here we use `::` to manually tell Haskell the type we want, just like we sometimes have to do for `read`. `pure` takes a value of any type and wraps it up inside an `Applicative`.  If it seems familiar, it should! It's the same function we used to wrap values up into IO values earlier!

The other function all `Applicative`s have is `<*>`. Compare its type to the type of `fmap`:

```haskell
fmap  ::   (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
```

Can you guess what `<*>` does? It's just like `fmap`, but where `fmap` wanted a function and a value wrapped up in a functor, `<*>` wants a function and a value both wrapped up in an applicative. That's a mouthful, let's see it in action.

```haskell
Prelude> (+1) `fmap` (Just 10)
Just 11
Prelude> (Just (+1))  <*>  (Just 10)
Just 11
```

You could even chain it up even more.

```haskell
Prelude> (Just (+)) <*> (Just 1) <*> (Just 10)
Just 11
Prelude> Nothing <*> (Just (+))
Nothing
```

Here's how `Maybe` is made to be an instance of `Applicative`

```haskell
instance Applicative Maybe where  
    pure = Just  
    Nothing <*> _ = Nothing  
    (Just f) <*> x = fmap f x 
```

Unfortunately, we're unable to mix and match `Applicative`s like we might wish we could.

```haskell
Prelude> (Just (+)) <*> [1,2,3,4]
-- error!
```

But since both sides have to be the same type of `Applicative`, this means Haskell can figure out what types we want from just one argument.

```haskell
Prelude> (Just (+5)) <*> (pure 5)
Just 10
```

And it can also be chained, like this!marginnote(Recall the `take` function, for which `take 3 [1..]` evaluates to `[1,2,3]`. It take a list and truncates it.)!

```Haskell
Prelude> (Just take) <*> (Just 3) <*> (Just [1,2,3,4])
Just [1,2,3]
```

There's also `<$>`, equivalent to `fmap` but an operator, which allows us to use it in infix notation. 

```haskell
Prelude> fmap (+1) [1,2,3]
[2,3,4]
Prelude> (+1) <$> [1,2,3]
[2,3,4]
```

It's really useful because it combines well with `<*>`.

```haskell
Prelude> (++) "Haskell" "!"
"Haskell!"
Prelude> (++) <$> Just "Haskell" <*> (pure "!")
Just "Haskell!"

Prelude> take 5 [1,2,3,4,5,6,7,8,9,10]
[1,2,3,4,5]
Prelude> take <$> Just 5 <*> Just [1,2,3,4,5,6,7,8,9,10]
Just [1,2,3,4,5]
```

Notice how we use `<$>` first, to apply `fmap` from `(++)` to `Just "Haskell"` to get `Just ("Haskell"++)`. We then use `<*>` on `Just "!"` to apply `("Haskell"++)` to `"!"`. So if you want to use a normal function on values wrapped up in `Applicative`s (also called *applicative functors*), you can just use `<$>` and `<*>` as required. Don't worry if this seems confusing, try looking at the second example (with `take`) and try and work through it. 

---

__***Exercises***__:

1) Rewrite

    ```haskell
    take <$> Just 5 <*> Just [1,2,3,4,5,6,7,8,9,10]`
    ```
    
    so it uses `fmap` instead of `<$>`

2) Use `<$>` and `<*>` to apply `rem` to `Just 16` and `Just 4`. The result should be `Just 0`.

---

!newthought(To see) how applicatives work on lists, let's use `<*>` and `<$>` to generate some usernames. 

```haskell
Prelude> superlatives = ["Smelliest", "Steamiest", "Spiciest"]
Prelude> nouns = ["Malt", "Mold", "Meat"]
Prelude> (++) <$> superlatives <*> nouns
["SmelliestMalt","SmelliestMold","SmelliestMeat","SteamiestMalt","SteamiestMold","SteamiestMeat","SpiciestMalt","SpiciestMold","SpiciestMeat"]
```

What's happened here is `<*>` has applied every function in the List in the left-hand argument to every value in the List of the right-hand argument. We make these functions from `(++) <$> superlatives`, which turns all our superlatives into functions that prepend that superlative to another string. The result: A List of usernames that you're free to use next time you need to create an account for something. 


Here's what makes applicatives monoidal functors. Notice this similarity:

```haskell
($)   ::   (a -> b) ->   a ->   b
(<$>) ::   (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
```

Remember, `$` is function application. So `f $ x` is equivalent to `f x`. It takes a function and a value and applies that function to that value.

`<$>` is `fmap`. To use `Maybe` as an example, `f <$> (Just x)` is the same as `Just (f x)`. It take s a function and a functor, and applies that function to the contents of the functor,

`<*>` is `fmap`, only it works on applicatives and the function you pass it is also wrapped up. So `(Just f) <*> (Just x)` is equivalent to `Just (f x)`.

So, what exactly does `Applicative` have to do with `Monoid`? The technical term is it's a *monoidal functor*. The trick is, you see those `f`s that wrap up the values in `<*>`?

```haskell
(<*>) :: f (a -> b) -> f a -> f b 
```

Look at the `Maybe` type in particular.

```haskell
(<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b 
--       1st argument    2nd argument   output
```

`Maybe` can be `Just <something>` or `Nothing`. If the first argument is `Nothing`, or the second argument is `Nothing`, the output is `Nothing`. That can be thought of as an associative binary function! This is kind of vague for `Maybe` but for some types like `(String, a)` it's much more clear.

```haskell
Prelude> ("hello ", (4+)) <*> ("world", 10)
("hello world",14)
```

What's going on here is `<*>` is getting two 2tuples. The first one is of type `(String, Int -> Int)` and the second one is of type `(String, Int)`. It combines the two `String`s using `mappend`!marginnote(`mappend` for `String`s is just `++`.) and it applies the function in the first tuple to the value in the second tuple. This could be used for logging, for example:

```haskell
Prelude> ("Multiplying by 10. ", (10*)) <*> (("Adding 4. ", (4+)) <*> ("Original value 10. ", 10))
("Multiplying by 10. Adding 4. Original value 10. ",140)
```

The `String`s are combined with `mappend`, and the values on the right are mapped. This makes any 2-tuple with a `Monoid` in the first position an `Applicative`.

## Monads

!newthought(*Monads* have) a reputation of being difficult to learn. James Iry was making a joke when he said "A monad is just a monoid in the category of endofunctors, what's the problem?", but he wasn't totally right!sidenote(That'd be a better description for an applicative, although you can argue it applies to monads). 

Monads are pretty simple. Just like you can imagine functors being applicatives with extra features, you can imagine a monad being an applicative with extra features. Indeed,Everything in the `Monad` typeclass is also in the `Applicative` typeclass. Since everything in the `Applicative` typeclass is also in the `Functor` typeclass, that means that all monads are applicatives and functors! But they're far more powerful than either applicatives or functors, which is why you see them everywhere in Haskell, from lists to `IO` and even to the humble `Maybe`.

Here's a reminder for all the terms we've introduced in this chapter.

1) A functor is something that can be mapped over (using `fmap` or `<$>` in Haskell). Examples include Lists and `Maybe`. `fmap` takes a function and a functor, and applies the function to the contents of the functor. So `fmap (+1) (Just 3)` evaluates to `Just 4`. A functor must

2) An applicative is a functor, so it still has `fmap`, but it also has two additional operations!marginnote(Most functors are applicatives these days.). 

    1) It has `pure`, which takes a value and wraps it up in an applicative. So `pure 3 :: [Int]` evaluates to `[3]`, and `pure 3 :: Maybe Int`

    2) It has `<*>`, which is just like `fmap` except both the function being passed and the value to apply it to are both wrapped up in applicatives. So `Just <*> (Just 3)` evaluates to `Just 4` (Compare that to `(+1) <$> (Just 3)`, which also evaluates to `Just 4`).

To understand monads, many people find the use of analogies helpful. Monads usually provide *context* to a value, sometimes called *computational context*. They're used like modifiers to other types. For example, you may have a `String`. If this `String` represents someone's licence plate, it might not always exist, such as when that person does not have a car. So you might choose to represent this with a `Maybe String`, so my licence plate would be `Just "ABCD123"` and my friends who take the subway would be `Nothing`. `Maybe` has given us some context for the `String`, we've said it may not exist. But you may have noticed an issue here - what if someone has multiple cars! That would call for a different context, the context of *zero or more*. To represent that, we'd use a `[String]`. These are the monads we're going to be spending most of our time discussing - `Maybe` and `[]`. 

That should give you a little bit of an intuition for monads, so it's time to get specific. To start us off, let's look at the `Monad` typeclass definition.

```haskell
class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    return :: a -> m a
```

These functions should look pretty familiar. `return` is just `pure`, which you should recognize from `Applicative`s!marginnote(`return` is a relic of the time Haskell developers didn't know about applicatives, it only still exists for backwards compatibility reasons. `pure` will work with any `Applicative` while `return` will only work with `Monad`s, so `pure` is strictly better.). It takes a value and wraps it up in an `Applicative` (or in this case, not just any `Applicative`, but a `Monad`). 

The other function monads provide us is `>>=` the bind operator. There's also `>>`, but it's just a simpler version of `>>=` so we'll skip over it for now.

Look at the type signature for `>>=`:

```haskell
(>>=) :: m a -> (a -> m b) -> m b
```

It's actually pretty simple. The goal of `>>=` is to take a value `m a`, and a function from `a` to `m b`, and apply that function to the value. How would we do that for `Maybe`? Here's how!

```haskell
Nothing >>= f = Nothing  
Just x >>= f  = f x
```

Easy as pie! That's all we need for the `Maybe` monad. Let's put this to the test. Do you remember our function `maybeHead`?

```haskell
-- maybeHead.hs
!include(haskelltests/should_compile/maybeHead.hs)
```

Here's how it works, in case you forgot.

```haskell
Prelude> maybeHead [1,2,3]
Just 1
Prelude> maybeHead []
Nothing
```

This takes a `[a]` and returns a `Maybe a`. But what if we wanted to pas it a `Maybe [a]`? We do this with `>>=`!marginnote(Compare `a -> m b` to `[a] -> Maybe a`. `maybeHead` takes a parameter of a certain type (`[a]`), and returns a parameter of a different type (`a`) wrapped in a `Maybe`.).

```haskell
Prelude> xs = Just [1,2,3]
Prelude> xs >>= maybeHead
Just 1
```

`>>=` unwrapped the `[a]` out of the `Maybe [a]` and passed it to `maybeHead`, which returned `Just 1`. But if our `Maybe [a]` was `Nothing`, this is what would have happened:

```haskell
Prelude> xs = Nothing
Prelude> xs >>= maybeHead
Nothing
```

If the left hand of `>>=` is `Nothing`, that's what the output will be. For proof of this, we can try it with `undefined`, a special value which we can pass to any function but that function isn't allowed to look at or use:

```haskell
Prelude> Nothing >>= undefined
Nothing
```

When you think about it, this is exactly what we want! `Maybe` means there's a chance our value doesn't exist. Once it's `Nothing`, we don't want to do anything with it anymore, we just want everything after it to be `Nothing`.

How can we use this? Remember our `maybeDivide` function?

```haskell
-- maybeDivide.hs
!include(haskelltests/should_compile/maybeDivide.hs)
```

Here's how it works, in case you've forgotten. It divides two numbers, and returns `Just <result>`. However, if the denominator is `0`, it will return `Nothing`.

```haskell
Prelude> maybeDivide 2 4
Just 0.5
Prelude> maybeDivide 2 0
Nothing
```

Can we use this with `>>=`? Let's see!

```haskell
Prelude> xs = Just [2,3,4]
Prelude> xs >>= maybeHead >>= (maybeDivide 3)
Just 1.5
```

This won't crash, even if you have things that would normally cause issues, such as empty lists or division by `0`!

```haskell
Prelude> Nothing >>= maybeHead >>= (maybeDivide 3)
Nothing
Prelude> (Just []) >>= maybeHead >>= (maybeDivide 3)
Nothing
Prelude> (Just [0]) >>= maybeHead >>= (maybeDivide 3)
Nothing
```

If one of these functions outputs `Nothing` at any stage, the output will be `Nothing`. This is quite an elegant way to handle errors because we only have to check once, at the end. You can see how `>>=` makes working with `Maybe` much more convenient.

Let's try some silly examples using lambdas!marginnote(A lambda is a function you can write in an expression, without giving it a name. Their format is `\parameter1 parameter2 -> <expression>`. They're very useful when working with functions like `>>=`, which expects a function of type `a -> m b` for its right-hand argument.).

```haskell
Prelude> (Just 0) >>= (\x -> Just (x + 1)) >>= (\x -> Just (x * 3)) >>= (\x -> Just (show x)) >>= (\x -> Just ("And the answer is " ++ x))
Just "And the answer is 3"
```

Each of these lambdas we've made takes a plain value and returns a `Maybe` value. `>>=` does all the work of extracting the value out of the `Maybe a` and passing it to our lambda. Here's how we might do it if we were writing a program. Remember though that this example is silly, and is only to show off the fact that `>>=` is unwrapping our values for us.

```haskell
-- sillyMaybeBindTest.hs
!include(haskelltests/should_compile/sillyMaybeBindTest.hs)
```

If we really wanted to do all this though, we should do it with something called `do` expressions. All `do` notation does is take away the hard work of typing out all our lambdas by hand. 

```haskell
-- sillyMaybeDoTest.hs
!include(haskelltests/should_compile/sillyMaybeDoTest.hs)
```

Notice that we don't use an `x <-` for the last line!marginnote(You may notice that this looks like something you might expect from imperative programming languages. It's this similarity that's caused some to say "Haskell is the world’s finest imperative programming language" (That, in addition to Haskell's impressive ability to abstract over imperative ideas, such as `while` and `for` loops). That said, it's generally best to use an imperative programming language if you intend to mostly be using the imperative programming style.). That's because, like `>>=`, the result of `do` is the result of the last line. It doesn't make sense to give a name to the output of the last line. You don't need to have an `x <-` for any of the lines, although you won't be able to reference previous values if you don't.

If we wanted to, we could use `pure` instead of `Just`. As long as we use `Just` at least once, Haskell will know we want the output of our `do` expression to be a `Maybe` value, so it will call the version of `pure` that returns a `Maybe`.

```haskell
-- sillyMaybeDoTestPure.hs
!include(haskelltests/should_compile/sillyMaybeDoTestPure.hs)
```

Of course, the result of all these is the same:

```haskell
*Main> result
Just "And the answer is 3"
```

By the way, reusing previous names in `do` expressions is called *shadowing* and it is generally considered bad practice. If you can't think of good names, something like this is always an option!marginnote(My preferred way to type subscripts like `₁` is to use the Latex Input extension in VSCode, which allows me to simply type `\_1`):

```haskell
-- sillyMaybeDoTestSub.hs
!include(haskelltests/should_compile/sillyMaybeDoTestSub.hs)
```

Note that if *any* of these evaluate to `Nothing`, the whole thing will evaluate to `Nothing`.

```haskell
-- sillyMaybeDoTestNothing.hs
!include(haskelltests/should_compile/sillyMaybeDoTestNothing.hs)
```

```haskell
*Main> result
Nothing
```

You may think this is strange, since `y <- Nothing` seems like it shouldn't influence the result at all. But consider what would happen if you then tried to use `y`. When you write `x <- Just 3`, `x` becomes `3` (Not `Just 3`). What would `y` be?

## Monad Laws

!newthought(Like functors) and monoids, monads have laws that your `Monad` instances should obey.

The first law is that `pure x >>= f` should be equivalent to `f x`. Remember, `pure` just wraps a value up in some other type, like `Maybe`. Then `>>=` unwraps it and passes it to `f`. Together, they undo each other.

The second law is that `m >>= pure` should be equivalent to `m`. This is because `>>=` is responsible for unwrapping `m` and passing it to the function on the right (in this case `pure`). Then `pure` wraps it right back up. This means they should undo each other the other way, too.

The third law is that `(m >>= f) >>= g` should be equivalent to `m >>= (\x -> f x >>= g)`. What it's basically saying is that monads should be associative - the parentheses shouldn't really matter!marginnote(This might remind you of monoids, and that's because monads and monoids are closely related in category theory. If you're interested in how exactly, read [Category Theory for Programmers](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/).).

# More Monads

## The List Monad

!newthought(In the) last chapter, we discussed the `Maybe` monad. But there are other monads as well, such as `[]`. Here's its definition:

```haskell
instance Monad [] where  
    xs >>= f = concat (fmap f xs)  
```

So, you pass `>>=` a list `[a]` and a function `a -> [b]`. It applies the function to every item in the list (creating a list of lists), then concatenates all the lists together. Here's a demonstration:

```haskell
*Main> [1,2,3] >>= \x -> [x, x*1000]
[1,1000,2,2000,3,3000]
```

The `1` in `[1,2,3]` is turned into `[1, 1000]`. The `2` is turned into `[2, 2000]` and the `3` is turned into `[3,3000]`. All these lists are concatenated together to get `[1,1000,2,2000,3,3000]`.

A natural way to think about this is functions that can return multiple values. Imagine a `sqrt` function. As you might be aware, `sqrt` has two values that would make sense to return - a positive and a negative. There's a function in Haskell called `sqrt` that squares a number and returns the positive, let's make `sqrt'` which does it right.

```haskell
sqrt' x = let root = sqrt x 
          in [root, -root]
```

Now, let's test it out!

```haskell
Prelude> sqrt' 4
[2.0,-2.0]
```

Now, imagine we wanted to find every 4th root. We can just apply `sqrt'` twice!

```haskell
Prelude Data.Complex> sqrt' 4 >>= sqrt'
[1.4142135623730951,-1.4142135623730951,NaN,NaN]
```

Well... that's disappointing. When you run `sqrt` on a negative number, it returns `NaN`!marginnote(If you run `sqrt` on a complex number  it will return the correct result, not `Nan` (complex numbers are available in `Data.Complex` and made using the `:+` operator).). 

Hopefully this communicates what the list monad is doing. Monads aren't anything super complicated or hard - they're just an additional context for a value.

# Important Information For Using Haskell

!newthought(This chapter) will tell you the last few things you still need to know to become a genuine Haskell dev. After this, you'll have the skills you need to start being really productive! A project we'll be making soon is going to be called `irc`, so go ahead and run `stack new irc` somewhere to generate it (notice we don't use `stack new irc simple` - we'll be using some features not in the `simple` template). We'll actually be making a little IRC client! If you're not familiar, IRC is an internet protocol for making online chat rooms. Anyone can host an *IRC server*, which can have multiple chat rooms inside it called *channels*.

## Getting 3rd-Party Code With Stack

!newthought(You may) remember the tool we used earlier, Stack. Stack isn't a package manager, it's responsible for building your code, which occasionally involves downloading packages from the internet. It doesn't try to manage the packages your program uses, it just builds what are called "targets" (and their dependencies). To build a target, you want to use `stack build <target>`. You very rarely want to use `stack install`. `stack install` is for installing Haskell *programs* to your computer, it's totally different from `npm install` or `pip install`. You **probably don't want to use `stack install`** (It almost got removed entirely to eliminate this exact confusion).

To add a dependency, you write its name in the `package.yaml` file under the `dependencies` section. Let's add one called `lens` (Lens is a library that has many useful things related to getting and setting values). We'll also add one called `hspec` and `QuickCheck` for testing our project, and one called `network` which we'll use to communicate to IRC servers.

Go to our `irc` folder and look at `package.yaml`. Change the `dependencies:` line (line 22) from

```yaml
dependencies:
- base >= 4.7 && < 5

```

To

```yaml
dependencies:
- base >= 4.7 && < 5
- hspec
- QuickCheck
- lens
- network
```

This change tells `stack` that our code depends on a library called `hspec`, a library called `lens`, and a library called `network`. `base` is the basic stuff all Haskell programs use, like numbers (the `>=4.7 && < 5` tells cabal which versions of `base` it can expect our code to work for).

To test it out, run `stack ghci` while in the `irc` folder. This will build your project and open a new GHCi instance from which you can import libraries you've added to your project. 

By default, our program just prints the text `somefunc`. Let's test that out by typing `main` (remember, `main` is the function run by Haskell when you turn it into an .exe).

```haskell
*Main Lib> main
someFunc
```

Also, we have access to the libraries we just imported. To test that out, import the Lens library with `import Control.Lens`. It's useful for getting values from tuples.

```haskell
*Main Lib> import Control.Lens
*Main Lib Control.Lens> view _1 ("hello", "goodbye", "how")
"hello"
```

You'll see that confusingly, Lens indices for tuples start at 1. 

Let's briefly discuss the structure of this project. The `main` function for our project is in `app/Main.hs`. It imports the `Lib` module which we define in `src/Lib.hs`. Inside `Lib.hs` we define a function called `someFunc`, so we're able to call it inside `Main.hs`. 

```haskell
-- app/Main.hs
module Main where

import Lib

main :: IO ()
main = someFunc
```

```haskell
-- src/Lib.hs
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
```

The first couple lines in `src/Lib.hs` are new to us. They're written like:

```haskell
module Lib
    ( someFunc
    ) where
```

But that's just for ease of input, they could have also written it like this:

```haskell
module Lib (someFunc) where
```

What this means is that `Lib` only *exports* the `someFunc` function (when you import a module, you only get what it exported). By default modules export everything, adding `(someFunc)` tells it to only export `someFunc`. Only exporting what you have to is considered good practice in Haskell.

The idea is that all of our little helper functions go in `Lib`, and then our actual code to run our app goes in `Main`. This is because any code in the `src` folder can be automatically tested, but code in the `app` folder cannot. Next, we'll write our automatic tests in `Spec.hs`.

## How To Test Our Code

You may be familiar with the Stack command `stack build`. It's what we used to build our `textAdventure` project into an executable we could distribute to our friends and family. We could run this executable to test our project manually, but that's a bit repetitive. A more modern Haskell-ey way is to *write our tests in code*, then have those run. The library we do that with is one called `hspec`, which we set up in the last chapter.

To run our tests, we just run `stack build --test` (or just `stack test` for short). This will build our code and then run the tests. `stack build` can be pretty slow because GHC spends a lot of time optimizing, so you can use `stack build --fast --test` instead. But you shouldn't use `fast` to make a production build since it trades fast builds for slow code. If you run this, you should see:

```haskell
Test suite not yet implemented
```

That's because we haven't added any tests yet! Let's write our first function and then test it. As a reminder, we're making a program that'll let us talk in IRC chatrooms. We'll let the user enter a *channel* (the IRC term for a chatroom) which our program will join and talk in. Channels in IRC must begin with a `#` and can't contain a space. Let's start writing a function that will take a `String` and return a `Bool` telling us whether it's a valid name for a channel. We'll do this in `src/Lib.hs`, so we can test it.

```haskell
-- src/Lib.hs
module Lib
    ( someFunc,
      isValidChannelName -- We also want to export isValidChannelName, so we add it here.
    ) where


-- We'll just check if the first letter is '#'.
isValidChannelName :: String -> Bool
isValidChannelName s = (head s) == '#'

someFunc :: IO ()
someFunc = putStrLn "someFunc"
```

Now, let's write a test for this! Go into `test/Spec.hs`.

```haskell
-- test/Spec.hs
main :: IO ()
main = putStrLn "Test suite not yet implemented"
```

Not much to it. Add `import Test.Hspec` and `import Lib` to the top.

```haskell
-- test/Spec.hs
import Test.Hspec
import Lib

main :: IO ()
main = putStrLn "Test suite not yet implemented"
```

This will give us access to the `hspec` functions, which we'll use for testing the functions in `Lib`. Then, we'll add a test. Tests are easy to understand once you've seen them.

```haskell
-- test/Spec.hs
import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
    describe "Lib.isValidChannelName" $ do
      it "allows channels with # in the first character" $ do
        isValidChannelName "#haskell" `shouldBe` (True :: Bool)
        isValidChannelName "#ghc" `shouldBe` (True :: Bool)
```

`hspec` uses a lot of nested `do`s. Let's add another test to make sure we don't just accept anything.

```haskell
-- test/Spec.hs
-- [...]
main :: IO ()
main = hspec $ do
    describe "Lib.isValidChannelName" $ do
      it "allows a channel with # in the first character" $ do
        isValidChannelName "#haskell" `shouldBe` (True :: Bool)
        isValidChannelName "#ghc" `shouldBe` (True :: Bool)

      it "disallows channels without # in the first character" $ do
        isValidChannelName "haskell" `shouldBe` (False :: Bool)
```

Now, run `stack build --fast --test` and see what it says! Both of our tests will pass. This kind of testing is known as *spec testing*, and is very useful. But it has some flaws: we have to write every test ourselves, which is time-consuming. Also, we might miss some edge case we didn't think about! That's where *property testing* comes in, with `QuickCheck` (which comes with `Hspec`). Import `Test.Quickcheck` at the top.

```haskell
-- test/Spec.hs
import Test.Hspec
import Test.QuickCheck
import Lib
-- [...]
```

The idea behind property testing is instead of giving specific tests, you give a function that does a test based off a random input. Here's how it works:

```haskell
-- test/Spec.hs
-- ...

main :: IO ()
main = hspec $ do
    describe "Lib.isValidChannelName" $ do
      it "allows a channel with # in the first character" $ do
        isValidChannelName "#haskell" `shouldBe` (True :: Bool)
        isValidChannelName "#ghc" `shouldBe` (True :: Bool)

      it "disallows channels without # in the first character" $ do
        isValidChannelName "haskell" `shouldBe` (False :: Bool)
        
      it "allows channels should contain a '#'" $ do
        property $ \xs -> if isValidChannelName xs
                           then elem '#' (xs :: String)
                           else True
```

instead of using `shouldBe`, we've made a property. The property is a function that should always return True if your function works correctly. Now we test that if our function says something is a valid channel name, that channel name should contain a `#`. QuickCheck will generate a load of random values!marginnote(By default, QuickCheck does around 100 tests.) and pass them to your function, and if your function ever returns `False` it'll be counted as a test failure. Let's run our tests with `stack build --fast --test`. Lo and behold, we find a bug! 

```haskell
Failures:

  test\Spec.hs:14:7:
  1) Lib.isValidChannelName Allowed channels should contain a '#'
       uncaught exception: ErrorCall
       Prelude.head: empty list
       (after 1 test)
         ""
```

It says our function threw an *uncaught exception*, specifically, it throws an exception when we pass it an empty list! Let's fix our function.

```haskell
-- src/Lib.hs

isValidChannelName :: String -> Bool
-- Use pattern matching to return False when passed an empty list.
isValidChannelName [] = False          
isValidChannelName s = (head s) == '#'
```

Now when running `stack build --fast --test`, we get `3 examples, 0 failures`! Property tests with QuickCheck can be a powerful tool to validate that your programs run properly, in addition to spec tests with Hspec. Any time you fix a bug in one of your functions, you should write a spec or property test to make sure it doesn't pop up again. This is called regression testing, and it often comes easy, since if you think a function has a bug the first thing you should do is write some tests for it. 

You can also see how much of your code you've tested by telling stack to run a *code coverage report*. You can do this with `stack build --fast --test --coverage`. This should generate something like:

```haskell
irc-0.1.0.0: Test suite irc-test passed
Generating coverage report for irc's test-suite "irc-test"
 71% expressions used (5/7)
100% boolean coverage (0/0)
     100% guards (0/0)
     100% 'if' conditions (0/0)
     100% qualifiers (0/0)
100% alternatives used (2/2)
100% local declarations used (0/0)
 50% top-level declarations used (1/2)
```

The most important one is the last one - `50% top-level declarations used`. This essentially tests how many top-level functions (functions not inside another function) you've got at least one test for. Our `Lib` module has two, `someFunc` and `isValidChannelName`, and we've only tested the latter so that leaves us with 50% top-level declarations. We'll be getting rid of `someFunc` soon so we won't bother testing it.


Also, if you don't feel like writing a test, you can just write `pending`, like this.

```haskell
main :: IO ()
main = hspec $ do
    describe "Lib.isValidChannelName" $ do
      it "allows a channel with # in the first character" $ do
        isValidChannelName "#haskell" `shouldBe` (True :: Bool)
        isValidChannelName "#ghc" `shouldBe` (True :: Bool)

      it "disallows channels without # in the first character" $
        isValidChannelName "haskell" `shouldBe` (False :: Bool)
        
      it "allows channels should contain a '#'" $ do
        property $ \xs -> if isValidChannelName xs
                           then elem '#' (xs :: String)
                           else True

      it "disallows spaces" $ do
        pending
```

This is a helpful reminder that you still need to write that test (Stack will remind you when you run your tests). If there's some reason you haven't written the test besides laziness, you can use `pendingWith`.

```haskell
      it "disallows spaces" $ do
        pendingWith "This is an exercise for you, the reader! \
                    \Write a spec test to test if our function \
                    \allows a channel name with a space."
```

Now, replace this `pendingWith` with a spec test to see that our function won't accept a channel with a space like `"#haskell programming"`. We haven't implemented checking for spaces yet, so this test should fail until we do. It's always a good idea to write a test for features you haven't implemented yet, so you won't forget. Also, you may have noticed the backslashes inside this string. That's how you spread strings across multiple lines, which can be used to make your code easier to read.

---

__***Exercises***__:

1) Write a spec test that ensures that `isValidChannelName` returns `False` when passed an empty string.

2) Write a spec test that tests that `isValidChannelName` returns `False` when passed `"#learn Haskell"`. This test should fail.

3) Write a property test that takes a value `xs` and checks that `isValidChannelName (xs ++ " ")` is `False`. 

4) In `Lib.hs`'s `isValidChannelName`, use `not (elem ' ' s)` to check that the input `String` does not contain a space. You can do this with the binary `&&` operator, `if/then/else` syntax, or [Guards](../more-about-functions-useful-syntax#Guards). 

5) After completing #4, rerun your tests. They should pass!

---

## Reading and Writing Documentation

Often, we would like to explain what a certain function does, in a way that's not easily represented in the function's name. For example, maybe we would like to say that our `isValidChannelName` function will return `True` in cases where the first character is `#` and the `String` does not contain a space. This is called *documentation*. We can do this with a comment, but there's a special way to annotate our comments so they can be made into a pretty website, using a tool called *Haddock*.

Here's our `isValidChannelName` function, with a descriptive comment:

```haskell
-- |'isValidChannelName' takes a 'String' and will return 'True' if the 'String' begins with a '#' and does not contain a space. 
isValidChannelName :: String -> Bool
isValidChannelName [] = False
isValidChannelName s
   | (head s) /= '#' = False
   | (elem ' ' s)    = False
   | otherwise       = True
```

The `-- |` is special syntax that informs GHC that this is Haddock documentation. It must be placed before the type signature. We can also spread it out across multiple lines, like this:

```haskell
-- |'isValidChannelName' takes a 'String' and will return 'True'
-- if the 'String' begins with a '#' and does not contain a space. 
isValidChannelName :: String -> Bool
isValidChannelName [] = False
isValidChannelName s
   | (head s) /= '#' = False
   | (elem ' ' s)    = False
   | otherwise       = True
```

We can also use `-- ^` to give details about individual arguments and the return value:

```haskell
-- |'isValidChannelName' takes a 'String' and will return 'True'
-- if the 'String' begins with a '#' and does not contain a space. 
isValidChannelName :: String -- ^ The channel name.
                   -> Bool   -- ^ 'True' if the channel name is valid, 'False' otherwise.
isValidChannelName [] = False
isValidChannelName s
   | (head s) /= '#' = False
   | (elem ' ' s)    = False
   | otherwise       = True
```

There's a lot of special markup you can use to make your documentation prettier, you can read about it in the [Haddock documentation](https://haskell-haddock.readthedocs.io/en/latest/markup.html#markup).

Now, you can *build* your haddock documentation with stack using the `--haddock` flag. To build and test your code without optimization, plus build haddock, run `stack build --test --fast --haddock`. Now, Haddock often takes a long time to run, and you usually don't really care that much about seeing your own haddock documentation, but instead you're more interested in seeing the documentation of your dependencies (our current dependencies are `hspec`, `QuickCheck`, `lens`, and `network`). After the first time it's much faster to only build those, and we can do that with `haddock-deps`, since it won't run at all if we haven't added new dependencies. This is our new command:


```bash
stack build --test --fast --haddock-deps
```

Now, we can read the documentation for a dependency with `stack haddock --open <something>`. For example, we can look at the documentation for `lens` with `stack haddock --open lens`. However, it's written in a somewhat intimidating style so I recommend looking at `hspec` instead. This is better than reading the docs online because you're guaranteed to be reading the docs for the right version of whatever library you're using. 

You can also get a searchable version of these docs with *Hoogle*. You must first run a command to generate Hoogle's index:

```bash
stack hoogle -- generate --local
``` 

This will probably take some time, especially since it'll probably have to download hoogle. You'll have to rerun this whenever you add any more dependencies but it should be much faster after the first time. After this you just need to start a Hoogle server with one easy command:

```bash
stack hoogle -- server --local --port=8080
```

Then go to [http://localhost:8080](http://localhost:8080) and try doing a search! Maybe look at the type of the `property` function we used for writing property tests in QuickCheck. By the way, if you ever add a new dependency, you need to re-run `stack hoogle -- generate --local`. 

## Warnings and Editor Settings

GHC had the concept of "warnings" in addition to errors. This is when GHC sees things that it thinks might be the source of bugs in the future. Most warnings are turned off by default, but you can put something in your `package.yaml` to turn them on. Open your `package.yaml` find the dependencies section:

```yaml
dependencies:
- base >= 4.7 && < 5
- hspec
- QuickCheck
- lens
- network

```

We're gonna add something right below it:

```yaml
dependencies:
- base >= 4.7 && < 5
- hspec
- QuickCheck
- lens
- network

ghc-options:
- -Wall
- -Wcompat
- -Wincomplete-record-updates
- -Wincomplete-uni-patterns
- -Wredundant-constraints
```

This turns on the following warnings: `Wall`, `Wcompat`, `Wincomplete-record-updates`, `Wincomplete-uni-patterns`, and `Wredundant-constraints`. This isn't C++, if you see a warning you should fix it. You should try to never check in code with warnings to version control. This is under the section `ghc-options` because these are just regular options passed to GHC - you don't have to build your project with Stack, you could call GHC manually, but Stack handling everything for you makes things much more convenient and reliable.

Next up is the topic of GHC language extensions. Haskell is a very nice language but sometimes it has some room for improvement through unofficial extensions implemented in GHC. To do this, we're going to add another section beneath `ghc-options`, called `default-extensions`. You can turn an extension on by writing a special comment at the top of your source files, but this turns them on for all files and is much more convenient. This is a long list of extensions, and lots of them are fairly complicated so I'm not going to explain them all yet. This list is from Alexis King, and some of the extensions are too complicated for me to easily explain here. Most will be addressed through the course of this book, though, so stay tuned. If you're interested in what a particular extension does, you can look it up on [the GHC docs](https://downloads.haskell.org/~ghc/8.2.2/docs/html/users_guide/glasgow_exts.html). 

```yaml

ghc-options:
- -Wall
- -Wcompat
- -Wincomplete-record-updates
- -Wincomplete-uni-patterns
- -Wredundant-constraints

default-extensions:
- BangPatterns
- ConstraintKinds
- DataKinds
- DefaultSignatures
- DeriveFoldable
- DeriveFunctor
- DeriveGeneric
- DeriveLift
- DeriveTraversable
- DerivingStrategies
- EmptyCase
- ExistentialQuantification
- FlexibleContexts
- FlexibleInstances
- FunctionalDependencies
- GADTs
- GeneralizedNewtypeDeriving
- InstanceSigs
- KindSignatures
- LambdaCase
- MultiParamTypeClasses
- MultiWayIf
- NamedFieldPuns
- OverloadedStrings
- PatternSynonyms
- RankNTypes
- ScopedTypeVariables
- StandaloneDeriving
- TupleSections
- TypeApplications
- TypeFamilies
- TypeFamilyDependencies
- TypeOperators
- TemplateHaskell
- QuasiQuotes
```

## What Are All These Files, Anyway

You may have noticed we have 3 different files here, `stack.yaml`. `package.yaml`, and `irc.cabal`. Here's the good news: you don't have to worry about `irc.cabal`. In fact, you can delete it if you want (although it will reappear). Let me explain.

In the beginning, there was just GHC. If you wanted to compile a program, you passed a list of all the files and all the settings to GHC, and GHC gave you an executable. Of course, this isn't very good from a usability perspective. Because of that, Cabal was created. Cabal is a program which reads a file like `irc.cabal` and uses the information inside that file to tell GHC to build a program. You can tell it what options you want to provide GHC, what files to build, what your dependencies are, etc. and Cabal would automatically manage it all for you. But Cabal had lots of issues, and a poor user interface, so Stack was created. This added an additional file, `stack.yaml`, where you could write some extra information and then Stack would handle calling Cabal for you, and downloading packages, running tests, and some other things. You rarely need to mess with `stack.yaml`, thankfully. But then some people realized that Cabal's file format was actually pretty bad - it's verbose and often requires you write the same thing multiple times. That's why the Hpack file format was created - it's just a file named `package.yaml` which gets converted to a `.cabal` file in a fairly straightforward process. This means you can edit the much nicer `package.yaml` instead of the messier `irc.cabal`. In practice, you'll spend most of your time editing `package.yaml`, occasionally editing `stack.yaml`, and you should never have to edit `irc.cabal`.

When you want to add a dependency, you put it in the `dependencies` section in your `package.yaml`. Stack then looks in a library called "Stackage LTS" for a library by that name and downloads that one if it finds it. There are multiple versions of Stackage LTS, and the one Stack looks at is determined by the *resolver* in `stack.yaml`. The reason for this complexity is for the goal of reproducible builds. You don't write the version number of the library you're using, you only write the name, and there's only one version of each library of any particular instance of Stackage LTS. If you'd like to use a different version, or a version not in Stackage LTS, you must also specify a version number in `stack.yaml`. You're essentially saying "if you have a need for this package, use this version". If you forget to do this, Stack will fail to build since it won't know which version to use, but it will give you a nice error message that will typically tell you what to add to the `extra-deps` section of your `stack.yaml` in order to fix it. In practice, most libraries you need should be in Stackage LTS so you don't have to edit `stack.yaml` too much.

Hopefully, this cleared up some lingering confusion you may have had. Now that we've explained that, let's write our IRC client!

## How to Develop and Debug

The nice thing about having automatic tests for all your code is that Stack makes it very simple to develop this way. You can add the `--file-watch` flag to your stack command, and this will cause Stack to automatically rebuild your code and rerun your tests every time you change your code. The full command with everything we've added is as follows:

```bash
stack build --test --fast --haddock-deps --file-watch
```

If you've been writing tests for all your functions, preferably before you even implement them, this becomes a very nice way to develop. Another option is to open your project in `stack ghci` and run your code with `main`, and reload your code with `:r` when you make a change. You can also run your actual project after you've built it with:

```bash
stack exec irc-exe
```

Change `irc` to something else if your project is named differently. Eventually, there will be a `stack run` command which should remove the need to build before running. 

Debugging in Haskell is somewhat difficult. The general process should be:

1) Observe a bug.

2) Figure out how to reproduce the bug consistently.

3) Try to think about what functions are involved in that bug.

4) Think about what those functions inputs and outputs should be in the case that causes the bug.

5) Start writing tests for how your function *should* work - when these tests fail, you've probably identified one bug or another.

6) Change your functions to fix whatever was causing the tests to fail.

7) Check if the bug still exists. If so, return to step 4.

One very helpful tool is the `trace` function. Inside `Debug.Trace`, the `trace` function takes a `String` and a value and returns that value. But invisibly in the background, it also prints the `String` you passed in. There is also `traceShow` which takes any value in the `Show` typeclass instead of a `String`. If you want to see if a function is being called, you can replace its output with `trace "yep, it's being called" <output>`. If a function is returning `x`, you can see that with `traceShow x x`. This is common so you can use `traceIdShow` which just prints and returns whatever you pass it. 

# Generalized Algebraic Data Types and Data Kinds

We're getting into some *advanced* Haskell features here. It's our first foray into Haskell's language extensions, and we're starting off with a good one. It's called Generalized Algebraic Data Types, or GADTs for short. Hopefully you remember data types, they look like this:

```haskell
data Bool = False | True  
```

Here, `Bool` is a type and `False` and `True` are values. Instead of plain values, you can also have *data constructors*, like so:

```haskell
data Currency = Dollars Double | Yen Double | Euros Double     deriving (Show)
```

Now `Dollars` is a function which takes an `Int` and returns a value of type `Currency`. 

```haskell
Prelude> Dollars 2
Dollars 2.0
```

We can pattern match on these, which is what makes them useful. Let's write a `convertToDollars` function.

```haskell
-- convertToDollars.hs
!include(haskelltests/should_compile/convertToDollars.hs)
```

We can use this to convert any `Currency` to `Dollars`.

```haskell
Prelude> convertToDollars (Yen 3000)
Dollars 26.7
```

To reiterate, `Yen`, `Euro` and `Dollars` are special functions called *data constructors*. In our case, they take a `Double` and return a value of type `Currency`.

So that's is data constructors, but we can also have type constructors. These are pretty useful, especially for situations like `Maybe`. Here's how `Maybe` is defined:

```haskell
data Maybe a = Nothing | Just a  
```

`Maybe` is a *type constructor*, which is similar to a a function but it takes a type and returns a type. `Just` is a *data constructor*, which takes a value of type `a` and returns a value of type `Maybe a`.

```haskell
Prelude> :t Just "hello"
Just "hello" :: Maybe [Char]
```

This should seem pretty familiar. We've also discussed *recursive data structures*. A recursive data structure can contain itself, which is useful for making data structures that can be infinitely large, such as a *List*. Here's how we'd do that to make a data structure capable of holding a list of `Int`s.

```haskell
data IntList = End | Cons Int IntList     deriving (Show, Read, Eq)  
```

This seems pretty complicated, but it's actually quite simple. `IntList` is the name of our type. There are two ways to construct one. The first is with just a plain `End`. 

```haskell
Prelude> End
End
Prelude> :t End
End :: IntList
```

The second is by passing `Cons` an `Int` and another `IntList`.!marginnote(`Cons` stands for *constructor*.)

```haskell
Prelude> Cons 3 End
Cons 3 End
Prelude> Cons 3 (Cons 4 End)
Cons 3 (Cons 4 End)
Prelude> Cons 3 (Cons 4 (Cons 5 End))
Cons 3 (Cons 4 (Cons 5 End))
```

Notice that our list always ends with `End`. Of course, we can make our `IntList` work with all types by using an algebraic data type.

```haskell
data List a = End | Cons a (List a)     deriving (Show, Read, Eq)  
```

Which is very convenient, since it saves us from having to make a new list type manually every time we need one.

```haskell
Prelude> Cons 'a' End
Cons 'a' End
Prelude> Cons 3 End
Cons 3 End
```

This is actually very similar to how lists in Haskell are actually implemented! But they use `:` instead of `Cons` and `[]` instead of `End`.

```haskell
Prelude> 3:[]
[3]
Prelude> 3:4:5:[]
[3,4,5]
```

Type constructors take some types and return a new type, and GADTs drastically increase the power of type constructors. To test it out, we need to tell GHC that we want to enable `GADTs`. We do this by adding something called a *language pragma* to the top of our file. In practice, we just write `{-# LANGUAGE <Extension> #-}` at the top of our file. Our extension is `GADTs`, so we use `{-# LANGUAGE GADTs #-}`!marginnote(We can also specify this in our `package.yaml`, which makes the extension available to our whole project without having to write any special extensions.). The most basic thing we can do with GADTs is have what's called an *uninhabited type*, a type with no values. These are pretty much only useful with GADTs so they're not available outside of that.

```haskell
-- GADTsTest.hs
!include(haskelltests/should_compile/GADTsTest1.hs)
```

`Uninhabited` is a type that contains no values!marginnote(`Uninhabited` actually does contain the special value `undefined`, which is contained by all types. But we'll ignore `undefined` for now.)  This means `Uninhabited` is almost entirely useless. Functions need to do two things: take and return values. If a function takes a value of type `Uninhabited`, we can never call it, because we'd have to pass it a value of type `Uninhabited` and there aren't any. And if a function returned type `Uninhabited`, there'd be no way to even write it! For this reason it's called a *phantom type*, a type which exists but we can't use it to write any useful functions. 

But we can use phantom types in a useful way, with type constructors! Those take types, not values, so the fact that our type doesn't have any values isn't an issue. `Maybe` is a type constructor which takes one types, so we can have `Maybe Int`, `Maybe Char`, and even `Maybe Uninhabited`. We can actually make a value of type `Maybe Uninhabited`, too!

```haskell
Prelude> :l GADTsTest1.hs
*Main> myStrangeValue = (Nothing :: Maybe Uninhabited)
```

`Maybe` takes a value of type `a`, but we don't need a value of type `a` to make a `Nothing`, so this works (although there's still not a good reason to use the type `Maybe Uninhabited` anywhere).

A common use of GADTs is to use an uninhabited type as a "tag" for another type, which is possible because the GADTs extension allows us to use `where` to make data constructors more powerfully than we could before. This allows us to put more information into the type system, which results in safer functions and fewer bugs in our code. To demonstrate, we'll write a new `List` type which will use two phantom types as tags to keep track of whether the list is `Empty` or `NonEmpty`. We can then use that to write a version of `head` which will only work on nonempty lists.

```haskell
-- SafeHead.hs
!include(haskelltests/should_compile/SafeHead.hs)
```

Don't worry about the specifics of `where` but just yet, let's jump to actually using `List`.

Let's make a `List` of `Char`s.

```haskell
Prelude> :l SafeHead.hs
*Main> myList = Cons 'a' (Cons 'b' End)
```

`myList` is a value of type `List`. The `List` contains two elements, `'a'` and `'b'`. But if you use `:t` to inspect the value of `myList`, you can see it's got the type `NonEmpty` in its second parameter.

```haskell
*Main> :t myList
myList :: List Char NonEmpty
```

Our function `safeHead` takes a value of type `List a NonEmpty`. This means we can pass it `myList`.

```haskell
*Main> safeHead myList
'a'
```

It successfully extracted the first type! `safeHead` is pattern matching against `Cons a b`, and returning `a`. This works because `a` will be the first value in our list, and `b` will be the rest of the list (which is `Cons 'b' End`). Running `safeHead End` would give us a compile error, which is exactly what we want. Any time you can make your code give you a compile error instead of a runtime error is a win!

Now, let's dig into the actual type definitions.

```haskell
data Empty
data NonEmpty

data List a tag where
        End  :: List a Empty
        Cons :: a -> List a tag -> List a NonEmpty
```

`data Empty` and `data NonEmpty` are pretty simple - they're phantom types which contain no values. They're used so we can "tag" our `List` type with useful values. The interesting definition is actually the `List` type, because it uses some syntax we haven't seen before. Earlier in this chapter we defined `List` with `data List a = End | Cons a (List a)`, where `End` and `Cons` were the data constructors. GADTs lets us do this in a more powerful way, with `where`. We still define two data constructors, `End` and `Cons`, but because we're using the new `where` Syntax we get to specify the output type of our data constructor. 

Before, `End` was a list of type `List a`. Now, `End` is a list of type `List a Empty`. 

Before, `Cons` took an `a` and a type `List a` and returned a `List a`. Now, `Cons` takes a value of type `a` and a value of type type `List a tag` and returns a value of type `List a NonEmpty`.

This may all seem very confusing, so don't worry if it seems a bit hazy. There's another language extension that makes GADTs much more useful, called `DataKinds`, which we'll introduce now and hopefully reinforce GADTs. It makes it very easy to write a safe `tail` function, which is what we'll try next.

`DataKinds` lets us use a value where Haskell expects a type by putting a `'` in front of it!marginnote(It also lets you use a type where haskell expects a *kind*, which is like the type of a type. We won't be discussing kinds just yet.). 

So for example, instead of saying a function takes an `Bool`, we could say it takes a `'True`!marginnote(GHC will sometimes allow us to leave out the `'` even when referring to `True` as a type, but it's best practice to leave it in.). Haskell will then check at compile time that this function never gets passed a `False`. This becomes very useful when combined with GADTs because it makes it allows us to put a lot of useful information into our types.

```haskell
-- SafeHeadTail.hs
!include(haskelltests/should_compile/SafeHeadTail.hs)
```

Now, what on earth is going on here? Well, first we make a simple recursive data type called `Nat`. This is short for *natural number*, which is a counting number like 0, 1, 2, 3, etc. Instead of having it encoded like that though, we make representations of these numbers with the data constructors `Zero` and `Succ Nat` (`Succ` is short for *successor*). We can stack as many `Succ`s on to a `Zero` as we want to make any natural number. 

```haskell
Prelude> :l SafeHeadTail.hs
*Main> one = Succ Zero
*Main> four = Succ (Succ (Succ one))
```

Then we have our definition of `List`, which uses `n` instead of `tag`. `n` is going to represent a `Nat`, which will represent the length of our list. `End` is much the same as before, except instead of `NonEmpty` we use `'Zero`. Now, observe `Cons` because it's very interesting:

```haskell
    Cons :: a -> List a n -> List a ('Succ n)
```

It takes an `a`, and a `List a n`, and returns a new `List a ('Succ n)`. Essentially it adds an extra `Succ` to the value which represents the length of our list! This means our lists start at length `Zero` and then get a `Succ` added to them every time we grow the list by one element. This means we can use information about the length of our list in type signatures, as we do in `safeHead` and `safeTail`!


`safeHead`'s type signature is `safeHead :: List a ('Succ n) -> a`. The `('Succ n)` will make it so it won't work if our list's `n` is `Zero`, but will work if it's `Succ Zero` or `Succ (Succ Zero)`. This is serving the same purpose as `NonZero` in the last example.

`safeTail`'s type signature is `safeTail :: List a ('Succ n) -> List a n`. This is the thing we couldn't do when we only had `Empty` and `NonEmpty`. `SafeTail` returns a list of all the values except the first, and it properly represents that the new list is one element shorter (it has one less `Succ`). 

Let's see these in action!

```haskell
*Main> :l SafeHeadTail.hs
*Main> myList = Cons 1 (Cons 2 End)
*Main> safeHead . safeTail $ myList
2
```

Now, what if we made an error and accidentally tacked on one too many `safeTail`s? If we were just using `tail` this would fail at runtime, but since we've encoded the length in the type system it fails at compile time instead, which is much better!

```haskell
*Main> safeHead . safeTail . safeTail $ myList
-- compile error, *not* a runtime exception!
```

This has been just a brief taste of the `GADTs` and `DataKinds` extensions. They're very powerful and we've only scratched the surface.