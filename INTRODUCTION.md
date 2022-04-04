## Introduction to Cognate

### Install
Currently, Cognate will run on Linux and MacOS systems. If you use Windows 10, then you can install Cognate on the Windows Subsystem for Linux. To build Cognate, you will need make, flex, bison, clang, llvm, libblocksruntime-dev, and libgc. After installing dependencies, simply run this command to build the compiler and runtime.
```
make
```
If that succeeds, install the compiler with the following command:
```
sudo make install
```
This installs cognate to the /usr/local prefix. To install to a different directory, use this command:
```
sudo make PREFIX=/my/prefix/dir install
```
You should then run the test script to test Cognate's functionality. This should work regardless of operating system.
```
make test
```

### First program

```
Print the string 'hello world!' to the screen;
```

Now to seems rather verbose doesn't it? Except is isn't. Cognate ignores all words starting with lower-case letters in what is called "informal syntax". The above programs are all equivalent to this:

```
Print 'hello world!';
```

```
please Print 'hello world!' thank you;
```

```
jlkfdajlf Print djklsajlk 'hello world' jdklsajdsja;
```

This feature is intended to allow you to embed comments into your statements, making code self documenting and more readable. Of course for simple programs like hello world, this documentation is unnecessary.

### Statements and the stack

A Cognate program is divided into 'statements', separated by semicolons. This may seem familiar to users of C or Java, but the way that Cognate evaluates these statements will likely not be.

```
~~ This is a comment by the way

Print 'hello';   ~~ This is the first statement
Print 'goodbye'; ~~ This is the second statement
```

Multiple statements can be put on the same line. The statements themselves are evaluated right to left, such that the statement `Print 'hello world';` would first evaluate `'hello world'` by pushing it to the stack, and then call `Print`.

But what is the stack? The stack is where Cognate stores intermediary values between expressions. When I write a constant such as `'hello world'`, it is put on top of the stack. When I call a function such as `Print`, it removes (pops) the value on the top of the stack and prints it. The stack persists between statements, allowing me to write:

```
put 'hello world' on top of the stack;
Print it;
```

Remember words starting with lower-case are ignored. For more information about stack oriented programming, I recommend reading [this](https://www.forth.com/starting-forth/1-forth-stacks-dictionary/), which discusses the stack in the context of the Forth programming languages, but many of the concepts map onto Cognate.

### Arithmetic and other functions

Cognate supports other data structures than strings - numbers for a start. Try writing:

```
Print the number 12.5;
```

Cognate arithmetic functions are not implemented traditionally as infix operators, but instead they are functions like `Print`.

```
Print + 1 2;  ~~ Adds 1 and 2, prints
Print * 3 4;  ~~ Multiplies 3 by 4, prints
Print / 7 49; ~~ Divides 49 by 7
Print - 9 18; ~~ Subtracts 9 from 18
```

This may feel unintuitive at first, but this notation means that we don't need operator precedence, and that we can use brackets for something else later.

### Variables

Cognate allows binding lexically-scopes variables with `Let`, which removed a value from the stack as an initialiser.

```
Let X be 'hello, I am a variable!';
Print X;
```

Cognate allow variable shadowing.

```
Let X be 12;
Print X;
Let X be 42;
Print X again;
```

Cognate also allows (but discourages) mutating variables with `Set`.

```
Let X be 'hello';
Print X;
Set X to 'mutated';
Print X again;
```

### Blocks

Cognate allows creating anonymous functions (closures) by wrapping them in braces. A block can be evaluated with the `Do` function.

```
Do (
  Print 'hello from inside a block!'
);
```

Blocks can capture variables from their outer scope.

```
Let X be 42;

Do (
  Print X
);
```

Notice that the last statement in a block does not require a terminating semicolon.

### Functions

Functions are defined with `Def`, which acts like `Let`, but requires a block as an argument.

```
Def Say-hello as (
  Print 'hello from a function!'
);

Say-hello;
```

Function arguments and return values are passed on the stack.

```
Def Multiply-by-three as (
  Let N be from the stack;
  return * 3 N;
);

Print Multiply-by-three 4;
```

Of course, we can write this more simply without a variable in what is called point-free form.

```
Def Multiply-by-three as (* 3);

Print Multiply-by-three 4;
```

### If statements

Before we understand if statements, you should probably know that `True` and `False` represent Cognate's booleans respectively.

```
Print True;
Print False;

Print < 1 2;  ~~ Prints True if 2 is less than 1, False otherwise
Print > 1 2;  ~~ Prints True if 2 is greater than 1, False otherwise
Print == 1 2; ~~ Prints True if 2 is equal to 1, False otherwise
Print /= 1 2; ~~ Prints True if 2 is not equal to 1, False otherwise
```

`If` is in fact a function, it takes three arguments. The first is a boolean, the second is a value to return if the boolean is `True`, the third is a value to return if the boolean is `False`. The below program will print the larger of the two variables `X` and `Y`.

```
Let X be 4;
Let Y be 9;

Print If < X Y then X else Y;
```

We can also chain `If` statements together.

```
Print If == X Y then 'X is equal to Y'
      If >  X Y then 'Y is greater than X'
      else      then 'X is greater than Y';
```

We can then use blocks and `Do` to have conditional execution.

```
Do If == 2 3 then (Print '2 is equal to 3')
             else (Print '2 is not equal to 3');
```

### Lists

The function `List` takes a block as an argument, and returns a list of the stack after executing it. This can be seen as a more powerful version of python's list comprehension.

```
Print List (1 2 3 4);
Print List (+ 1 2 and * 3 4);
Print List (Print 'hello world'; 5);
```

Cognate has many list functions

```
Push 1 to List (2 3 4);
Print;

First of List (1 2 3 4);
Print;

Rest of List (1 2 3 4);
Print;

Empty? List (1 2 3 4);
Print;
```

Cognate also has `Map`, which applies a block to each list element and creates a resulting list, and `Filter` which applies a block to each elements and removes it from the list if it returns `False`.

```
Map (* 2) over the List (1 2 3 4);
Print;

Filter (> 5) over the List (1 2 3 4 5 6 7 8 9 10);
Print;
```

`For` is like `Map` in that it applies a block to each list element, but it does not form a resulting list. `Range` creates a list of numbers. These functions together can be used to make a traditional for loop.

```
For each in Range 0 to 30 step 1 (
  Let I be our index;
  Print I;
)
```

Or written in a point-free style...

```
For each in Range 0 to 30 step 1 (Print);
```

Cognate does not yet have functions for the length of a list, or getting the nth list element. Implementing these with the above functions is left as an exercise for the reader.

### This tutorial is not finished!

Feel free to ask me about anything you do not understand or anything this tutorial does not cover, such as:
- groups
- stack manipulation functions
- pattern matching

There are example programs in the `examples` directory, and others in the `tests` directory. There is also a list of all functions in `compiler/builtins.c`.
