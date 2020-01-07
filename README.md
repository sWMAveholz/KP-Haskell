## Setup

To run the program install the Haskelly VS Code extension and stack.
For futher instructions use these [install instructions](https://marketplace.visualstudio.com/items?itemName=UCL.haskelly).

After installing please clone this repository and open it in your VS Code Studio.

## Usage

You have to build and run the program with ```stack```. The command line ```Boolparser> ``` appears and here you can enter your expression. For the boolean value Ture use ```T```, ```True``` or ```true```. For the boolean value False you can use any value. The Output is the parsed expression you entered and the result of the evaluation.

### Example Call

**Call**: ```Boolparser> T```  
**Output**: ```(Var "T",True)```

**Call**: ```Boolparser> !T&(F|T)```  
**Output**: ```(And (Not (Var "T")) (Or (Var "F") (Var "T")),False)```

**Call**: ```Boolparser> !True&   (true  |T)```  
**Output**: ```(And (Not (Var "True")) (Or (Var "true") (Var "T")),False)```

To end the Programm use Ctrl + d.

# Comparison of Haskell to Go

## Table of Contents
1. [Introduction](#intro)
2. [Haskell Overview](#HaskellOver)
	1. [Functional programming basics](#Haskellprogramming)
	2. [Haskell concepts](#Haskellconcepts)
3. [Parser Example](#ParsExample)
    1. [AST](#AST)
    2. [Library](#Library)
    3. [Module](#Module)
    4. [BoolParser](#BoolParser)
4. [Comparison](#Comp)
5. [Conclusion](#Conc)
6. [References](#Ref)

<a name="intro"></a>
## 1. Introduction

This document is about a project for the lecture "concepts of programming languages" at the Rosenheim University of Applied Sciences.
In this document the programming languages Haskell and Golang are compared. To do this a boolean parser example is used.

<a name="HaskellOver"></a>
## 2. Haskell Overview

Haskell is an advanced and purely functional programming language. Haskell is based on the lambda calculus. This is a formal mathematical system for expressing the notion of computation.

Haskell can be used for a variety of applications especially for programs which need to be highly modifiable and maintainable. The code is shorter and you have a rigorous control of side effects. This eliminates many unforseen interactions.

In this section you get a introduction to Haskell. 

<a name="Haskellprogramming"></a>
### 1. Functional programming basics

As mentioned above Haskell is a functional language. The focus is on what should be computed. Aspects like how to allocate memory or specify the order of executed commands are unimportant. This is a distinguishing characteristic of functional programming languages.

<a name="Haskellconcepts"></a>
### 2. Haskell concepts

There are many Haskell concepts. In this section some of them are explained. On this [website](http://ndrgrnd.net/posts/haskellOneSentence.html) you get a summary of more concepts of the programming language Haskell.

#### Immutability

All Haskell expressions that, once they are assigned to a name, they cannot change their value. So all expressions in Haskell are immutable. This brings that refactoring is easy and the code is easier to reason about. Data structures provide methods that create a copy of the old object.

#### Statically typed and Type inference

The type of every expression is determined at compile time. Haskell has a strong, static type system. The compiler can usually infer the types of expression. So in the following expamle the compiler can determine the type of every expression.

```Haskell
func baseAmt str = replicate rptAmt newStr
  where
    rptAmt = if baseAmt > 5 
      then baseAmt 
      else baseAmt + 2
    newStr = “Hello “ ++ str
```

In this expamle no types are assigned, but the compiler knows that `baseAmt` is an integer and `str` is a string. Also the type of `rptAmt` is known in this example because all `if` statments must have an else brunch and the result of both brunches must have the same type.

Most of the time it is possible to avoid type signatures, but in generally a signature is given for the top level functions. Type Driven Development is also possible in Haskell and it is not unreasonable to write out the types of functions before implementing them. So here is the example comleted by the signature:

```Haskell
func :: Int -> String -> [String]
func baseAmt str = replicate rptAmt newStr
  where
    rptAmt = if baseAmt > 5 
      then baseAmt 
      else baseAmt + 2
    newStr = “Hello “ ++ str
```

#### First class functions

In Haskell functions are not different than any other data used in a program. The functions can be used as arguments and as returned as values from other functions. This allows you to write functions that effectivly write other functions.

#### Purely functional

All functions you can write in Haskell is a funtion in the mathematical sense. Pure means that Haskell prohibit side effects. It is not possible to mutate local or global variables and access state like time or random numbers. Haskell works only with expressions and there are no statements or instructions. Have a look at the following example:

```Haskell
square :: Int -> Int
square x = x * x
```

The function `square` cannot mutate its arguments and there are no side effects.

#### Lazy

Lazy evaluation describes a method to evaluate a Haskell program. The evaluation of expression take place when their results are needed by other computations. This means the evaluation is deferred and the expressions are not evaluated when they are bound to variables. Arguments are evaluated when their values are actually used and the functions itself do not evaluate their arguments. You can use this feature of Haskell by defining a control structur.

```Haskell
when p m = if p then m else return ()
main = do args <- getArgs
          when (null args)
               (putStrLn "No args specified!")
```

In this expamle the evaluation for the values in the `when` function takes place in the `main` function. The lazy evaluation method only evalates the expression that are used in the program.

#### Monads

Finally to complete this section some words about monads in Haskell.

Monads in Haskell can be defined as composable computation descriptions. A monad is composed of functions. It encodes the control flow which allows pure functions to be strung together. For futher informations to this concept have a look at this [website](https://wiki.haskell.org/Monad).

<a name="ParsExample"></a>
## 3. Parser Example

In this section the boolean parser expamle will be explained. At first the abstract syntax tree will be shown. After this you find a short introduction to the MegaParsec library and in the last part the boolean parser will be completed.

<a name="AST"></a>
### 3.1. Abstract syntax tree

The abstract syntax tree for this boolean parser is defined as the following data structure:

```Haskell
data Node = Or Node Node 
  | And Node Node 
  | Not Node 
  | Var String 
  deriving (Show, Eq, Ord)
```

As you can see the type `Node` has four manifestations. It could be an `And` or an `Or`. Both of these manifestions consists of a tuple of Nodes. It is also possible that the type `Node` is a `Not`, which consists of one Node. Finally the last expression, that `Node` could be a `Var` of the type string. 

The second part of the abstract syntax tree is the evaluation function. On the basis of the above shown data structure `Node` the following `eval` function is written.

```Haskell
eval :: Node -> Bool
eval (Or x y) = eval x || eval y
eval (And x y) = eval x && eval y
eval (Not x) = not (eval x)
eval (Var x) = varTF x
```

The `eval` function is defined as a recursive function and evaluates the `Var` Nodes. 

Finally here is the `varTF` function which helps to evaluate the expressions:

```Haskell
varTF :: String -> Bool
varTF "T" = True
varTF "True" = True
varTF "true" = True
varTF x = False
```

Based on this function you can see which expressions you can use for the boolean parser.

<a name="Library"></a>
### 3.2. Library

The MegaParsec Library is used in the boolean parser example. In this section you find an example how to use the MegaParsec Parser type.

To start workting with the library it is common to define the type `Parser` as follows:

```Haskell
type Parser = Parsec Void Text
```

Then it is possible to write type signatures like the following:

```Haskell
pVariable :: Parser Node
pVariable = Var <$>
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")
```

#### Module

The `Control.Monad.Combinators.Expr` module is used in the boolean parser example.

This module only exports the `Operator` data type and the `makeExprParser` helper.

The `Operator` specifies operators that work on values of type `Node` as you can see in the following code:

```Haskell
operatorTable :: [[Operator Parser Node]]
operatorTable =
  [ [ prefix "!" Not ]
  , [ binary "&" And ]
  , [ binary "|" Or ]
  ]
```

An operator is either an unary prefix or a binary infix. The `operatorTable` is a nested list. It is a list of the operators and they are all in one inner list. This means they have all an equal precedence.

`prefix` and `binary` are functions and work also with the `Operator` data type. 

```Haskell
prefix :: Text -> (Node -> Node) -> Operator Parser Node
prefix  name f = Prefix  (f <$ sym name)

binary :: Text -> (Node -> Node -> Node) -> Operator Parser Node
binary  name f = InfixL  (f <$ sym name)
```

As you can see the parser handles prefix signs. To get the final result of the type `Node` we run `sym name` and return function to apply to terms in order. For `binary` the code is similar and works analogously to the `prefix` function.

`sym` is a parser that matches given text using `string` internally and then similarly picks up all trailing white space.

```Haskell
sym :: Text -> Parser Text
sym = L.symbol sc
```

To build the expression parser `pExpr` the operator from the above shown `operatorTable` are used.
The `makeExprParser` helper of the mentioned module is here usefull. 

```Haskell
pExpr :: Parser Node
pExpr = makeExprParser pTerm operatorTable
```

This function build an expression parser for the term parser `pTerm`.

```Haskell
pTerm :: Parser Node
pTerm = choice
  [ parens pExpr
  , pVariable
  ]
```

In this example there is no need for a `try` in `pTerm` because in case of an opening parenthesis `(` an expression will follow and a letter is the start of an idenifier.

Finally the following code shows the `parens` function to parse parentheses:

```Haskell
parens :: Parser a -> Parser a
parens = between (sym "(") (sym ")")
```

<a name="BoolParser"></a>
### 3.3. Boolean Parser

In this section the last part of the Boolean Parser will be explained.

The grammar that is implemented has the following EBNF form:

```
<expression> ::= <term> { <or> <term> }
<term> ::= <factor> { <and> <factor> }
<factor> ::= <var> | <not> <factor> | (<expression>)
<or>  ::= '|'
<and> ::= '&'
<not> ::= '!'
<var> ::= '[a-zA-Z0-9]*'
``` 

This grammar is implemented as in the section above explained.

The final function to make it possible to parse expressions is the following one:

```Haskell
parseAST = parse pExpr "<stdin>"
```

Now it is possible to call this function and parse an expression.

```Haskell
main :: IO ()
main = print (parseAST "T|(T&T)")
```

<a name="Comp"></a>
## 4. Comparison to Golang

Golang also implements functional programming paradigms. In this section they will be compared to Haskell.

#### Immutability 

In Golang immutability is not enforced. So is not the same as in Haskell. 

In Haskell expressions are immutable. After they are evaluated expressions cannot change anymore. This makes refactoring easy. Most data structures provide methods creating a copy of the old object.

#### Type inference

Golang inferred the variable's type form the value on the right hand side by using the `:=` syntax. This is possible without specifiying an explicit type. 

```go
s := "b" // string
i := 42  // int
```

Haskell also own this feature. Concrete types are deduced by the type system whereever it is obvious.

#### First class functions

Through the example it is clear, that Haskell functions are first class functions.

These property own both, Haskell and Golang. 

#### Purely functional

Golang is a multi paradim programming language and is not a purely functional language. Golang have features that enables to apply functional priciples. 

Here is the biggest difference to Haskell. Haskell is a purely functional language.

#### Lazy

Here is Golang also different from Haskell. While Haskell uses lazy evaluation, Golang evaluates expressions at the moment when variables are assined. Haskell evaluates expressions deferred when they are actually used.

#### Boolean Parser

In this section the programming languages Haskell and Golang are compared using some parts of the boolean parser example.

In advance it is difficult to compare the implemented boolean parser because for the implementation in Haskell predefined functions and data structures were used as in the sections before explained.

The implementation of the boolean parser in Haskell differs in some points from the implementation in Golang.

Because in Haskell the `Control.Monad.Combinators.Expr` module is used, we can implement the Boolean Parser easly. With the `binary` and `prefix` function the `operatorTable` can be written while in Golang the functions to parse `And`,`Not` and `Or` are longer and to parse the grammar the functions are defined separately. The predefined function of Haskell are here helpfull. The grammar to parse expressions can be easly implemented through these functions.

Similarities can be found for example by parsing parentheses.

Here the function which is used to parse parentheses in Golang: 

```go
func parseAtom(Input Input) Result {
	return parseVariable.OrElse(expect("(").AndThen(parseExpression).AndThen(expect(")")).First().Second())(Input)
}
```

The following code shows how to implement the `parens` function in Haskell.

```Haskell
parens :: Parser a -> Parser a
parens = between (sym "(") (sym ")")
```

Both the boolean parser in Golang and in Haskell parse the same following grammar:

```
Atom := Variable
        | "(" ^ Expression ^ ")"
```

<a name="Conc"></a>
## 5. Conclusion

In conclusion the programming languages Haskell and Golang differ in many points.

To mention here is, that Haskell is a purely funtional language while Golang only apply functional priciples. From this many differences can be derived. Golang supports also other programming styles.

Only in some points the languages are similar. The here elaborated similarities are the type inferece and the first class functions.

<a name="Ref"></a>
## 6. References

* [Haskell Introduction](https://wiki.haskell.org/Introduction)
* [Lambda Calculus](https://wiki.haskell.org/Lambda_calculus)
* [Haskell type system](https://mmhaskell.com/blog/2016/12/5/7mkljzq7zy97d66zm4yvtn8v1ph502)
* [Haskell features](https://www.haskell.org/)
* [Haskell concurreny](https://wiki.haskell.org/Concurrency)
* [Lacy evaluation](https://wiki.haskell.org/Lazy_evaluation)
* [Text.Megaparsec documentation](https://hackage.haskell.org/package/megaparsec-8.0.0/docs/Text-Megaparsec.html)
* [Megaparsec tutorial](https://markkarpov.com/tutorial/megaparsec.html)
* [Haskell module documentation](http://hackage.haskell.org/package/parser-combinators-1.2.1/docs/Control-Monad-Combinators-Expr.html)
* [Haskell immutability](https://mmhaskell.com/blog/2017/1/9/immutability-is-awesome)
* [Golang type inference](https://tour.golang.org/basics/14)
* [Haskell type inference](https://wiki.haskell.org/Type_inference)
* [Haskell first class functions](https://freecontent.manning.com/learning-haskell-first-class-functions/)
* [Functional Go](https://medium.com/@geisonfgfg/functional-go-bc116f4c96a4)
* [Boolean Parser Go](https://github.com/jweigend/concepts-of-programming-languages)
