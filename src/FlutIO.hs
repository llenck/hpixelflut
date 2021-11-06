module FlutIO where

import Data.Maybe (catMaybes)
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Control.Monad

import Network.Simple.TCP

import FlutFmt

-- for debugging:
--sendS sock = print
sendS sock = send sock . pack

send_px s x y r g b = sendS s $ fmt_px x y r g b

write_image :: Socket -> [(Int, Int, Int, Int, Int)] -> IO ()
write_image s = mapM (\(a, b, c, d, e) -> send_px s a b c d e) >=> (\_ -> pure ())

--concatJustStrings :: [Maybe ByteString] -> String
--concatJustStrings = foldl (++) "" . map unpack . catMaybes

recv_exact :: Socket -> Int -> IO (Maybe String)
recv_exact sock n = do
    resp <- recvS sock n

    case resp of
        Nothing -> pure Nothing
        Just s -> do
            let new_len = length s

            if new_len == n
                then pure $ Just s
            else do
                more <- recv_exact sock (n - new_len)
                pure $ case more of
                    Nothing -> Nothing
                    Just more_s -> Just $ s ++ more_s

readHex :: String -> Int
readHex = read . ("0x"++)

recvS :: Socket -> Int -> IO (Maybe String)
recvS sock n = recv sock n >>= pure . fmap unpack

read_pixel_block :: Socket -> [(Int, Int)] -> IO [(Int, Int, Int)]
read_pixel_block s cs = do
    let cmds = map (uncurry fmt_rq) cs
    let resp_bytes = (+) (length cs * 7) $ foldl (\o n -> o + length n) 0 cmds
    mapM (sendS s) cmds

    resp <- recv_exact s resp_bytes
    case resp of
        Nothing -> pure []
        Just s -> do
            let colors = map (head . drop 3 . words) $ lines s
            let color_bytes = map (\s -> (take 2 s, take 2 $ drop 2 s, take 2 $ drop 4 s)) colors
            pure $ map (\(a, b, c) -> (readHex a, readHex b, readHex c)) color_bytes
