{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Week05.HW05
    ( getSecret
    , decryptWithKey
    , parseFile
    , getBadTs
    , getFlow
    , getCriminal
    , undoTs
    , writeJSON
    , doEverything
    , main
    ) where

import Data.Bits (xor)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Function (on)
import Data.List (foldl', maximumBy, sortBy)
import Data.Map.Strict as Map
import Data.Ord (comparing)
import System.Environment (getArgs)

import Week05.Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret original modified = do
    o <- BS.readFile original
    m <- BS.readFile modified
    return $ BS.pack $ Prelude.filter (/= 0) $ BS.zipWith xor o m

-- >>> getSecret dog-original.jpg dog.jpg

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key path = do
    encryptedtext <- BS.readFile (path ++ ".enc")
    let decryptedtext = BS.zipWith xor encryptedtext $ BS.cycle key
    BS.writeFile path $ BS.pack decryptedtext

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = decode <$> BS.readFile path

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs pathVictimList pathTransaction = do
    vf <- parseFile pathVictimList :: IO (Maybe [String])
    tf <- parseFile pathTransaction :: IO (Maybe [Transaction])
    return $ case (vf, tf) of
        (Just v, Just t) -> Just $ Prelude.filter (\Transaction{..} -> tid `elem` v) t
        (_, _) -> Nothing

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = Data.List.foldl' updateFlow Map.empty
  where
    updateFlow acc Transaction{from = sender, to = receiver, amount = amt} =
        Map.insertWith (+) receiver amt $
            Map.insertWith (+) sender (-amt) acc

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal flowMap = fst $ maximumBy (comparing snd) (Map.toList flowMap)

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m ids =
    let payee = sortBy (compare `on` snd) [x | x@(_, amount) <- Map.assocs m, amount < 0]
        payer = sortBy (compare `on` snd) [x | x@(_, amount) <- Map.assocs m, amount > 0]
        pay (i : is) ((fn, fa) : fs) ((tn, ta) : ts) =
            Transaction{from = fn, to = tn, amount = tot, tid = i} : pay is fs' ts'
          where
            tot = min fa ta
            fs' = if fa > ta then (fn, fa - ta) : fs else fs
            ts' = if ta > fa then (tn, ta - fa) : ts else ts
        pay _ _ _ = []
     in pay ids payer payee

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path xs = BS.writeFile path $ encode xs

-- Exercise 9 -----------------------------------------

doEverything ::
    FilePath ->
    FilePath ->
    FilePath ->
    FilePath ->
    FilePath ->
    FilePath ->
    IO String
doEverything dog1 dog2 trans vict fids out = do
    key <- getSecret dog1 dog2
    decryptWithKey key vict
    mts <- getBadTs vict trans
    case mts of
        Nothing -> error "No Transactions"
        Just ts -> do
            mids <- parseFile fids
            case mids of
                Nothing -> error "No ids"
                Just ids -> do
                    let flow = getFlow ts
                    writeJSON out (undoTs flow ids)
                    return (getCriminal flow)

main :: IO ()
main = do
    args <- getArgs
    crim <-
        case args of
            dog1 : dog2 : trans : vict : ids : out : _ ->
                doEverything dog1 dog2 trans vict ids out
            _ ->
                doEverything
                    "dog-original.jpg"
                    "dog.jpg"
                    "transactions.json"
                    "victims.json"
                    "new-ids.json"
                    "new-transactions.json"
    putStrLn crim
