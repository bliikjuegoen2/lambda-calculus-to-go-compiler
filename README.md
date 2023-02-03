# Lambda Calculus Compiler

## About

This project aims to build a compiler in Haskell for my super set of the Lambda calculus. The main focus is the compiler itself. Hence, runtime components, such as garbage collection, cross platform compatibility, developing a standard library, etc., is out of the scope for this project. Go has all the necessary runtime features while still being low level enough and having fast compile times which made it suitable as a compilation target.

## Language Specification

Each section will give an example and its equivalent in python as well as a brief explanation.

### Variables

Variable names can contain any non-whitespace characters that is not any of the following: `()[];:$<>`. In addition, variable names cannot start with a digit.

For example:

```
#@#erjnwodif!!!!1332
```

is a completely valid varibale name

### Whitespace

Whitespace is completely ignored.

### Ints

Intergers can contain underscores to seperate digits.

For example:

```
299_792_458
```

### Function Abstraction

```
[x] x
```

In Python:

```python
lambda x: x
```

Square brackets designate the arguments of that function. The expression following the list of arguments will be returned by the function.

### Function Application

```
f a b
```

In Python:

```python
f(a,b)
```

Similar to other functional languages such as Haskell. Paranthesis are optional and arguments are only seperated by whitespace. Currying is supported.

Infix notation can also be used.

the following statement is equivalent to the examples given above.

```
a <f> b
```

### Variable Assignment

```
set x a; y b in x y
```

In Python:

```python
x = a
y = b
x(y)
```

The variables are only in the scope of the expression that follows the `in` keyword.

### If Statement

```
if cond
then:
    x
else:
    y
end
```

In Python:

```python
if cond:
    x
else:
    y
```

The if statement is really an expression. It will evaluate to the expression in either the `then` or the `else` else block.

### Recursion

```
[rec x] 
    if x <lt> 2
    then:
        1
    else:
        x <*> $rec (x <-> 1)
```

In Python:

```python
def factorial(x):
    if x < 2:
        return 1
    else:
        return x * factorial(x - 1)
```

There is a slight difference between the two examples. In the first one, the function is completely *anonymous* while in the second it is bounded to *factorial*. `$rec` refrences the function itself, thus making it possible to do recursion on anonymous functions.

### Blocks

```
block
    f x
    g y 
    h z
end
```

In Python:

```python
f(x)
g(y)
h(z)
```

It strings together multiple statements

### Calling External Functions

```
builtin 2 stdlib Plus
```

the first argument is the number of arguments that this function takes. The second argument is the Go package where the function is located. The third argument is the name of the function in Go.

When implementing a function in Go, its arguments and return value must have the type `core.Wrapped`.

## Dependancies

- [GHCup](https://www.haskell.org/ghcup/)
- [Golang](https://go.dev/)

## Instructions

to run the compiler enter:

```
stack exec lambda-calculus-compiler-exe -- -o [output file] [source file]
```

you will then have to setup a go module for the output 
file.

Inside the directory of the output file:

```
go mod init [module name]
go mod tidy
```

Finally, to run the code enter:

```
go run [output file]
```