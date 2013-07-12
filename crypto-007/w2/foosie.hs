{- Foosie -}

import Data.Char
import Data.Bits
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

import Control.Monad ((>>=), return, liftM, replicateM)

import System.Random
import System.Exit
import System.Environment (getArgs)

import Crypto.Cipher.AES

data Mode = ECB | CBC | CTR deriving (Eq, Ord, Show, Read)

-----

bytesHexString :: String -> ByteString
bytesHexString = ByteString.pack . map fromIntegral . parseHexString where
    parseHexString [] = []
    parseHexString (h:l:cs) = parseHex h l : parseHexString cs
    parseHex h l = digitToInt h * 16 + digitToInt l

hexStringBytes :: ByteString -> String
hexStringBytes = map intToDigit . foldr (unpair . fromIntegral) [] . ByteString.unpack where
    unpair w a = (w `div` 16) : (w `rem` 16) : a

-----

key :: ByteString -> Key
key bs
    | ByteString.length bs == 16 = initKey bs
    | otherwise                  = error "AES-128: 128-bit key required"

randomBytes :: Int -> IO ByteString
randomBytes n = liftM ByteString.pack $ replicateM n $ getStdRandom random

padBytes :: Int -> ByteString -> ByteString
padBytes n bs = ByteString.append bs $ ByteString.replicate m (fromIntegral m) where
    m         = n - (size `rem` n)
    size      = ByteString.length bs

unpadBytes :: ByteString -> ByteString
unpadBytes bs = ByteString.take n bs where
    m = fromIntegral $ ByteString.last bs
    n = if m > 16
            then error "AES-128: more than 128 bits of padding not allowed"
            else ByteString.length bs - m

breakBytes :: Int -> ByteString -> [ByteString]
breakBytes n bs
    | ByteString.length bs > n = b : breakBytes n bs'
    | otherwise                = [bs] where
        (b, bs')               = ByteString.splitAt n bs

xorBytes :: ByteString -> ByteString -> ByteString
xorBytes a b = ByteString.pack $ ByteString.zipWith xor a b

addBytes :: ByteString -> Integer -> ByteString
addBytes bs n = ByteString.pack $ reverse $ addBytes' n $ reverse $ ByteString.unpack bs where
    addBytes' _ [] = []
    addBytes' n ws@(w : ws')
        | n == 0             = ws
        | otherwise          = w' : addBytes' n' ws' where
            w'               = w + (fromIntegral n)
            n'               = (n + (fromIntegral w)) `shiftR` (bitSize w)

encryptBlocks :: Mode -> ByteString -> Key -> [ByteString] -> [ByteString]
encryptBlocks ECB _ k  = map $ encryptECB k
encryptBlocks CBC iv k = scanl (\i -> encryptECB k . xorBytes i) iv
encryptBlocks CTR iv k = ((:) iv) . zipWith xorBytes (map (encryptECB k . addBytes iv) [0..])

encrypt :: Mode -> Key -> ByteString -> IO ByteString
encrypt mode k bs = do
    iv <- randomBytes 16
    return $ ByteString.concat $ encryptBlocks mode iv k blocks where
        blocks        = breakBytes 16 $ padBytes' mode bs
        padBytes' CTR = id
        padBytes' _   = padBytes 16

decryptBlocks :: Mode -> Key -> [ByteString] -> [ByteString]
decryptBlocks ECB k bs               = map (decryptECB k) bs
decryptBlocks CBC k (i : bs@(b : _)) = i `xorBytes` decryptECB k b : decryptBlocks CBC k bs
decryptBlocks CBC k _                = []
decryptBlocks CTR k (iv : bs)        = zipWith xorBytes bs $ map (encryptECB k . addBytes iv) [0..]

decrypt :: Mode -> Key -> ByteString -> ByteString
decrypt mode k = unpadBytes' mode . ByteString.concat . decryptBlocks mode k . breakBytes 16 where
    unpadBytes' CTR = id
    unpadBytes' _   = unpadBytes

main :: IO ()
main = getArgs >>= act where
    act [a, m, hk, hs] = let
        k              = key $ bytesHexString hk
        s              = bytesHexString hs
        in act' a (read m) k s >>= putStrLn . show
    act _              = usage >> exitSuccess
    act' "-e" m k s    = encrypt m k s
    act' "-d" m k s    = return $ decrypt m k s
    usage              = putStrLn "Usage: foosie (-e | -d) (CBC | CTR) (key-in-hex) (subject-in-hex)"
