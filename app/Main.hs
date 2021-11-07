module Main where

import System.IO
import System.Environment
import Control.Monad
import Data.Maybe

import Network.Simple.TCP
import Codec.Picture (readImageWithMetadata, convertRGBA8, pixelAt, PixelRGBA8(PixelRGBA8))
import Codec.Picture.Metadata as M (Metadatas, Keys(Width, Height), lookup)

import FlutFmt
import FlutIO (pixelflut, write_image, read_pixel_block)
import Rainbow

fromRight :: Either a b -> b
fromRight (Left _) = error "fromRight called on Left"
fromRight (Right a) = a

get_dims :: M.Metadatas -> Maybe (Int, Int)
get_dims meta = do
    w <- M.lookup M.Width meta
    h <- M.lookup M.Height meta
    pure (fromIntegral w, fromIntegral h)

read_pixels im w h = map (\(x, y) -> (x, y, pixelAt im x y)) [(x, y) | x <- [0..w - 1], y <- [0..h - 1]]

dump_rgba8 (PixelRGBA8 r g b a) = (r, g, b, a)

read_im :: FilePath -> IO [(Int, Int, Int, Int, Int)]
read_im path = do
    im_ <- readImageWithMetadata path
    let (im, meta) = fromRight im_
    let im8 = convertRGBA8 im
    let (w, h) = fromJust $ get_dims meta
    let pixels = read_pixels im8 w h
    let fmt_pixels = map (\(x, y, c) -> (\(r, g, b, a) -> (x, y, r, g, b, a)) $ dump_rgba8 c) pixels
    pure $ map (\(x, y, r, g, b, a) -> (x, y, fi r, fi g, fi b)) $ filter (\(x, y, r, g, b, a) -> a > 127) fmt_pixels
    where fi = fromIntegral

move_im dx dy = map (\(x, y, r, g, b) -> (x + dx, y + dy, r, g, b))
recolor_im (r, g, b) = map (\(x, y, _, _, _) -> (x, y, r, g, b))
black_im = recolor_im (0, 0, 0)

plot_fn f = [(x, floor y, r, g, b) |
    x <- [0..1280],
    let n_x = fromIntegral x / 1280,
    let y = 360 + f n_x * (-360),
    --let (r, g, b) = (255, 255, 255)]
    let (r, g, b) = rainbow $ floor y]

sine_plot _ = pure $ plot_fn (sin . (*6.283))

sine_plots :: [Socket -> IO [(Int, Int, Int, Int, Int)]]
sine_plots = do
    i <- [0..30]
    [\_ -> pure $ plot_fn (\x -> sin (x * 15 + i * 5))]

copy fx fy w h tx ty s = do
    let coords = [(x, y) | x <- [fx..fx + w], y <- [fy..fy + h]]
    let (dx, dy) = (tx - fx, ty - fy)
    pixels <- read_pixel_block s coords
    let im = map (\((x, y), (r, g, b)) -> (x + dx, y + dy, r, g, b)) $ zip coords pixels
    pure im

clear _ = pure $ [(x, y, 0, 0, 0) | x <- [0..1280], y <- [0..720]]


block x y w h r g b = [(x, y, r, g, b) | x <- [x..x + w], y <- [y..y + h]]

paddle_im = block 0 0 10 100 255 255 255

paddle :: [(Int, Int, Int, Int, Int)] -> (Socket, SockAddr) -> IO ()
paddle im (s, _) = do
    hSetBuffering stdin NoBuffering
    paddle_ s im 0
    pure ()
    where paddle_ s im c = do
            ch <- getChar
            let dy = case ch of
                    'w' -> -10
                    's' -> 10
                    'W' -> -50
                    'S' -> 50
                    _   -> 0

            let dx = case ch of
                    'a' -> -10
                    'd' -> 10
                    'A' -> -50
                    'D' -> 50
                    _   -> 0

            let n_im = {- recolor_im (rainbow c) $ -} move_im dx dy im
            write_image s $ black_im im
            write_image s n_im

            paddle_ s n_im (c + 1)

main = do
    --pixelflut "pixelflut.uwu.industries" "1234" $ [\_ -> pure $ block 0 710 1280 10 255 255 255]
    --pixelflut "pixelflut.uwu.industries" "1234" $ [clear]
    args <- getArgs
    im <- case (listToMaybe args) of
        Nothing -> pure paddle_im
        Just s -> read_im s

    connect "pixelflut.uwu.industries" "1234" $ paddle im
    putStrLn "Done :)"
