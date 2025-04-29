import System.Exit
import Test.Hspec

import qualified HW01Tests as W1
import qualified HW02Tests as W2
import qualified HW03Tests as W3
import qualified HW04Tests as W4
import qualified HW05Tests as W5
import qualified HW06Tests as W6
import Week1.Testing (runTests)

main :: IO ()
main = do
    let result = runTests $ W1.allTests ++ W2.allTests ++ W3.allTests
    case result of
        [] -> putStrLn "OK, all tests passed"
        failures -> die $ show failures

    hspec $ do
        W4.allTests
        W5.allTests
        W6.allTests
