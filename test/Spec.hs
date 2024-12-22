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