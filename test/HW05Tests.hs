module HW05Tests (allTests) where

import qualified Data.ByteString.Lazy as BS
import System.Directory
import Test.Hspec

import Week05.HW05

allTests :: Spec
allTests = describe "\nWeek05" $ do
    describe "\nHaskell Bank" $ do
        it "\n getSecret read a key" $ do
            key <- getSecret "src/Week05/dog-original.jpg" "src/Week05/dog.jpg"
            key `shouldSatisfy` (not . BS.null)

        after_ deleteDecrypted $
            describe "tests with decryption" $ do
                it "\n create decrypted file" $ do
                    key <- getSecret "src/Week05/dog-original.jpg" "src/Week05/dog.jpg"
                    decryptWithKey key "src/Week05/victims.json"

                it "\n get bad transactions" $ do
                    key <- getSecret "src/Week05/dog-original.jpg" "src/Week05/dog.jpg"
                    decryptWithKey key "src/Week05/victims.json"
                    badTs <- getBadTs "src/Week05/victims.json" "src/Week05/transactions.json"
                    badTs `shouldSatisfy` (not . null)

                it "\n track the flow of money from bad transactions" $ do
                    key <- getSecret "src/Week05/dog-original.jpg" "src/Week05/dog.jpg"
                    decryptWithKey key "src/Week05/victims.json"
                    badTs <- getBadTs "src/Week05/victims.json" "src/Week05/transactions.json"
                    case badTs of
                        Nothing -> expectationFailure "No Transaction provided"
                        Just ts -> getFlow ts `shouldSatisfy` (not . null)

                it "\n get the mane of the criminal" $ do
                    key <- getSecret "src/Week05/dog-original.jpg" "src/Week05/dog.jpg"
                    decryptWithKey key "src/Week05/victims.json"
                    badTs <- getBadTs "src/Week05/victims.json" "src/Week05/transactions.json"
                    case badTs of
                        Nothing -> expectationFailure "No Transaction provided"
                        Just ts -> getCriminal (getFlow ts) `shouldSatisfy` (not . null)

                it "\n undo the transactions " $ do
                    key <- getSecret "src/Week05/dog-original.jpg" "src/Week05/dog.jpg"
                    decryptWithKey key "src/Week05/victims.json"
                    badTs <- getBadTs "src/Week05/victims.json" "src/Week05/transactions.json"
                    case badTs of
                        Nothing -> expectationFailure "No Transaction provided"
                        Just ts -> do
                            nids <- parseFile "src/Week05/new-ids.json"
                            case nids of
                                Nothing -> expectationFailure "No id provided"
                                Just ids -> do
                                    let flow = getFlow ts
                                    undoTs flow ids `shouldSatisfy` (not . null)
  where
    deleteDecrypted = removeFile "src/Week05/victims.json"
