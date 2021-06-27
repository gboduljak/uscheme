# μscheme
![](readme-resources/uscheme.png)

## Intro

**μscheme** (micro Scheme) is a toy interpreter for the Scheme programming language, implementing a relatively (large) subset of [R5RS](https://schemers.org/Documents/Standards/R5RS/) language specification. The interpreter comes with an integrated REPL shell.
The implementation of the interpreter is based on the book [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) and a series of blog posts [Write Yourself a Scheme, Version 2.0]([https://wespiser.com/writings/wyas/00_overview.html]). Test cases used in unit tests are taken from [Berkeley's Structure and Interpretation of Computer Programs course](https://inst.eecs.berkeley.edu/~cs61a/sp20/). 

However, this implementation differs significantly from both resources mentioned above. The most important differences are in the implementation of lexical scoping and the structure of evaluator.

### Supported Scheme features
- arithmetic and list processing
- majority of language primitives for processing lists and strings (e.g **car**, **cdr**, **cons** and friends)
- lexical scoping using **let**, **define** and **set**
- **lambda** functions and **named procedures** (recursion supported)
- conditionals using **if** and **cond**
- sequential execution using **begin**
- displaying (printing) using **display**
- loading source code from files into the shell using **load**

Again, it is worth noting this is by no means complete implementation of [R5RS](https://schemers.org/Documents/Standards/R5RS/) language specification, but 'small enough' programs should work. 

For examples of 'small enough' supported programs, see the section **A few examples**.

For more information about testing, see Tests section.

For instructions to setup and run the interpreter with REPL, see Build & Run section.
### Partial bootstrapping
The interpreter  comes with an implementation of a subset of Scheme standard library, written in Scheme. The implementation is taken from [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours). When the REPL is loaded, the above mentioned standard library is loaded into the shell and is ready for usage.

### A few examples
For a better overview, all language features required to interpret the following two programs are supported.

> Computation of **ackermann function**
```scheme
(define ackermann (
    lambda (m n) (
        if (= m 0)
            (+ n 1)
            (if (= n 0)
                (ackermann (- m 1) 1)
                (ackermann (- m 1) (ackermann m (- n 1))))
    )
))
(ackermann 1 2)
(ackermann 3 3)
```

> Peter Norvig's **riff-shuffle**
```scheme
(define (combine f)
  (lambda (x y)
    (if (null? x) nil
      (f (list (car x) (car y))
         ((combine f) (cdr x) (cdr y))))))
(define zip (combine cons))

(define riff-shuffle (lambda (deck) (begin
    (define take (lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq))))))
    (define drop (lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq)))))
    (define mid (lambda (seq) (/ (length seq) 2)))
    ((combine append) (take (mid deck) deck) (drop (mid deck) deck)))))

(riff-shuffle (list 1 2 3 4 5 6 7 8))
```

### Currently unsupported Scheme features
- **let***, **letrec** and friends
- full numeric tower
- explicit tail recursion optimisation
- hygienic macros :(

## Architecture

### Lexical and syntactic analysis

Lexical and syntactix analysis is implemented using monadic parser combinators from the [Parsec](app/Main.hs) library. 

Lexer is implemented in [Lexer.hs](src/Lexer.hs) and Parser is implemented in [Parser.hs](src/Parser.hs). Both of them are implemented using parser combinators. Great resources for parser combinators are :
* Erik Meijer
* Parsec documentation

> Example from Lexer.hs
```haskell
boolean :: Parser LispVal
boolean = true <|> false
  where
    true = do
      lexeme (try (Parsec.string "#t") <|> try (Parsec.string "true"))
      return (Bool True)
    false = do
      lexeme (try (Parsec.string "#f") <|> try (Parsec.string "false"))
      return (Bool False)
```
> Example from Parser.hs
```haskell
vector :: Parser LispVal
vector = do
  openVec
  vec <- Vector <$> many expr
  closeParens
  return vec
```
### Semantic analysis
The semantic analysis is absent due to performance reasons and the nature of Scheme language.
Scheme is dynamically typed, so typechecking essentially requires evaluation. For this reason, I have decided not to have semantic analysis, but to detect and report errors such as type mismatch, unbound variable or invalid function calls **within** the evaluation stage.

Semantic errors are modeled with [LispError](src/LispError.hs) type:

```haskell
data LispError
  = NumArgs Int [LispVal]
  | TypeMismatch String LispVal
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | ParserError ParseError
  | Default String
  | DivideByZero LispVal
  deriving (Eq)
```
If present, semantic errors are thrown during the evaluation stage done in [Evaluator.hs](src/Evaluator.hs).

Semantic analysis results in an instance of [Ast](src/Ast.hs), which is the abstract syntax tree of expressions:
```haskell
data LispVal
  = DottedList [LispVal] LispVal
  | List [LispVal]
  | Vector [LispVal]
  | Atom String
  | Number Double
  | String String
  | Bool Bool
  | Nil
  | PrimitiveFunction {name :: String}
  | IOFunction {name :: String}
  | Lambda
      { args :: [String],
        body :: [LispVal],
        targetScopeId :: Int,
        varargs :: Maybe String
      }
  deriving (Eq)
```
### Lexical scoping 
The implementation of lexical scoping is based on the scope resolver with a symbol table, which differs from the 'ad hoc' solutions using **IO monad** with IO handles or a **Reader** monad presented in the resources mentioned. The main reason for an approach of using a custom scope resolver monad and a symbol table is the separation of concerns and expressivity, which result in a more faithful and testable implementation of lexical scoping. 

The competing solution of using **IO monad** is probably more convenient, but violates separation of concerns and is not considered a good practice. The competing solution of using the **Reader** monad is harder to test and even possibly incorrect. 

- Everything related to scoping is implemented in **Scoping module** located in **src/Scoping** folder.
- Two main components of scoping implementation are the [Scope](src/Scoping/Scope.hs) itself and the [ScopeResolver](src/Scoping/ScopeResolver.hs) monad

> Scope

**Scope** is a data type used to implement a 'single level' of a symbol table. It contains the parent scope id and a symbol table of declarations within itself. It exposes **extend** and **lookup** operations which enable lookup of local variable and installation of a new (variable name, value pair), called a **Binding**.

```haskell
data Scope = Scope
  { id :: ScopeId,
    parentId :: Maybe ScopeId,
    symbolTable :: Map String LispVal
  }
  deriving (Show)

extend :: Scope -> Binding -> Scope
extend scope (name, value) =
  Scope
    { id = id scope,
      parentId = parentId scope,
      symbolTable = Map.insert name value (symbolTable scope)
    }

lookup :: Scope -> String -> Maybe LispVal
lookup Scope {symbolTable} name = Map.lookup name symbolTable
```
> Scope Resolver

**ScopeContext** is used to capture global scoping information, it stores all scopes and currentScopeId which is the id of currently active scope during evaluation. 

```haskell
data ScopeContext = ScopeContext
  { scopes :: Map ScopeId Scope,
    currentScopeId :: ScopeId
  }
  deriving (Show)

```
**ScopeResolver** is a state monad of **ScopeContext**. Essentially, in each 
``` haskell
type ScopeResolver a = StateT ScopeContext IO a
```

```haskell

extend :: Binding -> ScopeResolver Scope
extend binding =
  do
    scope <- current
    scopes <- gets scopes
    let scope' = Scope.extend scope binding
     in do
          put
            ScopeContext
              { currentScopeId = id scope,
                scopes = Map.insert (id scope) scope' scopes
              }
          current

lookup :: String -> ScopeResolver (Maybe LispVal)
lookup name = do
  currentScopeId <- gets currentScopeId
  match <- lookupIn currentScopeId name
  case match of
    (Just (value, scope)) -> return (Just value)
    Nothing -> return Nothing
```


### Evaluation

Evaluation relies on the [EvalMonad](src/EvalMonad.hs), which is a fairly complicated data structure, defined as:

```haskell
type EvalMonad a = ExceptT LispError (StateT ScopeContext IO) a
```

Although the definition looks scary at the first glance, it basically defines the EvalMonad as a computation which may result in an error of type **LispError** holding the internal state of type **ScopeContext**. In addition to everything written before, evaluation computation may perform IO operations (e.g **display** or **load**). The computation is parameterised over any type, which results in enough flexibility to implement any evaluation use case.
However, evaluation mostly results in an instance of [Ast](src/Ast.hs).

Evaluation is implemented in [Evaluator](src/Evaluator.hs) which heavily relies on pattern matching over possible expression types of [Ast](src/Ast.hs). Usually, the expression type is detected in [Evaluator](src/Evaluator.hs) and the evaluation of recognised expression type is done in separate evaluator. For example, the function application is recognised in [Evaluator](src/Evaluator.hs):
```haskell
eval :: LispVal -> EvalMonad LispVal
eval expr@(List (head : args)) = Application.eval expr eval
```
and implemented in [Application](src/Evaluators/Application.hs) evaluator:
```haskell
eval :: LispVal -> (LispVal -> EvalMonad LispVal) -> EvalMonad LispVal
eval expr@(List (func : args)) evaluate = do
  if func `elem` [Atom "and", Atom "or"]
    then do applyAndOr func args evaluate
    else do
      funcToApply <- evaluate func
      case funcToApply of
        lambda@Lambda {} -> do
          evaledArgs <- mapM evaluate args
          applyLambda lambda evaledArgs evaluate
        primitive@PrimitiveFunction {} -> do
          evaledArgs <- mapM evaluate args
          applyPrimitive primitive evaledArgs
        io@IOFunction {} -> return io
        _ -> throwError (BadSpecialForm "invalid application expression: " expr)
```
This logic applies to majority of language constructs implemented.
### Testing

Testing is done using unit tests written in [HSpec] framework. 
Test cases test a variety of language constructs and are taken from [Berkeley's Structure and Interpretation of Computer Programs course](https://inst.eecs.berkeley.edu/~cs61a/sp20/). However, most of those tests are taken from a few books:
 - The Little Schemer
 - Structure and Interpretation of Computer Programs

You can see tests and specifications in the **test** folder.
Usually, there is a scheme source code file containing a sequence of operations which is loaded into the unit testing module. The output of interpreter is compared with hardcoded expected test case outputs.

A good example is the fibonacci recursion test.
> [Fibonacci input file written in Scheme (fib.scm)](test/tests/recursions/fib.scm)

```scheme
(define fib (lambda (n) (
    if (< n 3)
        1
        (+ (fib (- n 1)) (fib (- n 2)))
    )
))
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 10)
(fib 20)
```
> [Fibonacci test specification in HSpec (RecursionSpec.hs)](test/RecursionSpec.hs)
```haskell
recursionSpec :: Spec
recursionSpec = do
  describe "recursion tests..." $ do
    it "recursively evaluates fibonacci" $ do
      input <- liftIO (readFile "./test/tests/recursions/fib.scm")
      let emptyCtx = getInitialScopeContext
      case parse input of
        (Right tree) -> do
          let testingCtx = snd $ evaluateMany tree emptyCtx
          let results = map fst $ evaluateManyParallel tree testingCtx
          results
            `shouldBe` [ Right (Atom "fib"),
                         Right (Number 1),
                         Right (Number 1),
                         Right (Number 2),
                         Right (Number 3),
                         Right (Number 5),
                         Right (Number 55),
                         Right (Number 6765)
                       ]
        (Left error) -> expectationFailure (show error)
```
## Build & Run

To build & run this project, you will need a [Haskell Stack](https://docs.haskellstack.org/en/stable/README/).
The following sequence of commands should setup the project:
- clone the project
- run ```stack install``` in the root folder
- run ```stack build``` in the root folder

To start the interpreter, run ```stack run``` in the root folder.
You should see:

![](readme-resources/repl.png)


To load scheme source files into the REPL, use ``(load "{path to source file}")``.

To exit REPL type ```(quit)```.

## Possible improvements
- Enhance lexer and parser error description (not too difficult)
- Implement unsupported language features (quite tricky, especially macros)
- Support for multiline input in REPL (quite tricky)