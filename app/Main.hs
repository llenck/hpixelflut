module Main where

import System.IO
import Control.Monad
import Data.Maybe

import Network.Simple.TCP
import Codec.Picture (readImageWithMetadata, convertRGBA8, pixelAt, PixelRGBA8(PixelRGBA8))
import Codec.Picture.Metadata as M (Keys(Width, Height), lookup)

import FlutFmt
import FlutIO (pixelflut, write_image, read_pixel_block)
import Rainbow

fromRight :: Either a b -> b
fromRight (Left _) = error "fromRight called on Left"
fromRight (Right a) = a

get_dims meta = do
    w <- M.lookup M.Width meta
    h <- M.lookup M.Height meta
    pure (w, h)

read_pixels im w h = map (\(x, y) -> (x, y, pixelAt im x y)) [(x, y) | x <- [0..w], y <- [0..h]]

dump_rgba8 (PixelRGBA8 r g b a) = (r, g, b, a)

read_im path = do
    im_ <- readImageWithMetadata path
    let (im, meta) = fromRight im_
    let im8 = convertRGBA8 im
    let (w, h) = fromJust $ get_dims meta
    let pixels = read_pixels im8 (fromIntegral w) (fromIntegral h)
    pure $ map (\(x, y, c) -> (\(r, g, b, a) -> (x, y, r, g, b, a)) $ dump_rgba8 c) pixels

plot_fn f = [(x, floor y, r, g, b) |
    x <- [0..1280],
    let n_x = fromIntegral x / 1280,
    let y = 360 + f n_x * (-360),
    --let (r, g, b) = (255, 255, 255)]
    let (r, g, b) = rainbow x $ floor y]

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

paddle_im x y (r, g, b) =
    (block x y 10 100 r g b) ++
    (block x (y - 10) 10 10 0 0 0) ++
    (block x (y + 100) 10 10 0 0 0) ++
    (block (x-10) y 10 100 0 0 0) ++
    (block (x+10) y 10 100 0 0 0)

wand_im x y (r, g, b) =
    (block 30 y 1200 5 r g b) ++
    (block 30 (y+10) 1200 10 0 0 0) ++
    (block 30 (y-10) 1200 10 0 0 0)

paddle :: Int -> (Socket, SockAddr) -> IO ()
paddle off (s, _) = do
    hSetBuffering stdin NoBuffering
    paddle_ s off 310 0
    pure ()
    where paddle_ s x y counter = do
            ch <- getChar
            let new_y = flip mod 720 $ y + case ch of
                    'w' -> -10
                    's' -> 10
                    _   -> 0

            let new_x = flip mod 1280 $ x + case ch of
                    'a' -> -10
                    'd' -> 10
                    _   -> 0

            let im = paddle_im new_x new_y $ rainbow 0 counter
            --let im2 = wand_im new_x new_y $ rainbow 0 counter
            write_image s im

            paddle_ s new_x new_y (counter + 1)

main = do
    --pixelflut "pixelflut.uwu.industries" "1234" $ [\_ -> pure $ block 0 710 1280 10 255 255 255]
    connect "pixelflut.uwu.industries" "1234" $ paddle 1000
    putStrLn "Done :)"
