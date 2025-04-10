import System.Exit

import qualified HW01Tests as W1
import qualified HW02Tests as W2
import Week1.Testing (runTests)

main :: IO ()
main = do
    let result = runTests $ W1.allTests ++ W2.allTests
    case result of
        [] -> putStrLn "OK, all tests passed"
        failures -> die $ show failures
