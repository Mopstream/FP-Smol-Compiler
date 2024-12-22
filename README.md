# Лабораторная работа 4

Проект: Компилятор маленького функционального языка `Smol`

Автор: Голиков Андрей Сергеевич P34092 335126

Были разработаны парсер и виртуальная машина для языка под чутким руководством книги ["Implementing Functional Languages: a tutorial"]("https://storage.googleapis.com/google-code-archive-downloads/v2/code.google.com/ipaper/Implementing%20Functional%20Languages%20a%20tutorial.pdf") by Simon L Peyton Jones

## Запуск

```shell
stack build
stack exec lab4-exe <file with code> <log file>
```

## Синтаксис

```ebnf
program   ::= sc (';' sc)*
sc        ::= var+ = expr

expr      ::= let defns in expr
            | letrec defns in expr
            | expr1

expr1     ::= expr2 relop expr1
            | expr2
            
expr2     ::= expr3 '+' expr2
            | expr3 '-' expr3
            | expr3

expr3     ::= expr4 '*' expr3
            | expr4 '/' expr4
            | expr4
    
expr4     ::= aexpr+

aexpr     ::= var
            | num
            | Pack {num, num}
            (expr)

digit     ::= '0'-'9'
num       ::= digit+

var       ::= alpha varch*
alpha     ::= 'A'-'Z'|'a'-'z'
varch     ::= alpha|digit|_

```

Хотелось еще добавить `case` выражения и лямбда функции, но пока не получилось..


Также в языке поддерживаются функции SKI вычислений, функция композиции `compose`, и функция `twice`, дублирующая вызов функции аргумента

### Пример Программы
```haskell
fib n = if (or (n==0) (n==1))
         1 
         (1 + fib (n-1) + fib (n-2)) ;

main = fib 5
```

## Логика

- Исполнение функциональной программы - вычисленение выражения
- Выражение представляет собой граф
- Вычисленение это последовательность редукций на графе, обновляющих значения в узлах

### Пример

Код
```haskell
square x = x * x ;
main = square (square 3)
```

Сначала раскрывается внешний суперкомбинатор `main` в его дерево

```
main!   ->        @! 
                 / \
           square   @
                   / \
             square   3
```

Смотрим на корень, видим там операцию применения и идем смотреть левого ребенка

```
       @! 
      / \
     @   \
    / \___@
   *     / \
   square   3
```

Захотели применить `*` - нужно вычислить его аргумент

```
       @        ->         @
      / \                 / \
     @   \               @   \
    / \___@!            / \___@!
   *     / \           *     / \
   square   3               @   \
                           / \___3
                          *

```

Такое мы уже можем редуцировать

```
    @         ->        @       -> 81 
   / \                 / \
  @   \               @!  \
 / \___@!            / \___9
*     / \           *
     @   \
    / \___3
   *

```

## Реализация

Реализован LL1 парсер-комбинатор, код представлен в `src/Language/Parsing`

Реализованы вспомогательные модули для ассоциативного массива (`src/Util/Assoc.hs`) и кучи с сохранением статистики (`src/Util/StatHeap.hs`)

Реализация машины представлена в `src/Machine.hs`, ниже приведены основные типы

```haskell
type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStack = [Addr]

type TiDump = [TiStack]

type TiHeap = StatHeap Node

type TiGlobals = Assoc Name Addr

type TiStats = (Int, (Int, Int), Int)
```

Компилятор, представлен в `src/Compiler.hs`

## Тестирование

Сложно было придумать, как это нормально тестировать, поэтому были написаны несколько пробных программ (`examples/*`)

### fact.smol
```haskell
fac n = if (n==0) 1 (n * fac (n-1)) ;
main = fac 5
```

### fib.smol
```haskell
fib n = if (or (n==0) (n==1)) 1 (1 + fib (n-1) + fib (n-2)) ;
main = fib 5
```
### gcd.smol
```haskell
gcd a b = 
    if (a==b)
    a
    (if (a<b) (gcd b a) (gcd b (a-b))) ;

main = gcd 6 10
```


### Код тестов

```haskell
import Compiler

main :: IO ()
main = do
    fibAns <- getAns "examples/fib.smol"
    assertEquals (fibAns, 15) "Fibbonacci testing ..."

    factAns <- getAns "examples/fact.smol"
    assertEquals (factAns, 120) "Factorial testing ..."

    gcdAns <- getAns "examples/gcd.smol"
    assertEquals (gcdAns, 2) "GCD testing ..."


getAns :: String -> IO Int
getAns name = do
      prog <- readFile name
      let log = "/dev/null"
      run prog log

assertEquals :: (Eq a) => (a, a) -> String -> IO ()
assertEquals (a, b) testDesc =
  putStrLn $ (if a == b then "Passed: " else "Failed: ") <> testDesc
```
### Отчет тестирования

```
llab4> test (suite: lab4-test)
            
Progress 1/2: lab4=== RES ===
NNum 15
===========
Passed: Fibbonacci testing ...
=== RES ===
NNum 120
===========
Passed: Factorial testing ...
=== RES ===
NNum 2
===========
Passed: GCD testing ...
                  


lab4> Test suite lab4-test passed
Completed 2 action(s).
```

