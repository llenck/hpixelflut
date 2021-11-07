module Main where

import System.IO
import System.Environment
import Control.Monad
import Data.Maybe

import Network.Simple.TCP
import Codec.Picture (readImageWithMetadata, convertRGBA8, pixelAt, PixelRGBA8(PixelRGBA8))
import Codec.Picture.Metadata as M (Metadatas, Keys(Width, Height), lookup)

import FlutFmt
import FlutIO (conf_im, write_image, read_pixel_block)
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

block x y w h r g b = [(x, y, r, g, b) | x <- [x..x + w], y <- [y..y + h]]

clear = block 0 0 1280 720 0 0 0
border =
    (block 0 0 1280 10 255 255 255) ++    -- up
    (block 0 10 10 720 255 255 255) ++    -- left
    (block 10 710 1280 10 255 255 255) ++ -- down
    (block 1270 10 10 700 255 255 255)    -- right

paddle_im = block 0 0 10 100 255 255 255

paddle :: Bool -> [(Int, Int, Int, Int, Int)] -> (Socket, SockAddr) -> IO ()
paddle rb im (s, _) = do
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

            let moved_im = move_im dx dy im
            let n_im = if rb
                then recolor_im (rainbow c) moved_im
                else moved_im

            write_image s $ black_im im
            write_image s n_im

            paddle_ s n_im (c + 1)

rotat_l :: (Int, Int) -> (Int, Int)
rotat_l (0, 1) = (-1, 0)
rotat_l (-1, 0) = (0, -1)
rotat_l (0, -1) = (1, 0)
rotat_l (1, 0) = (0, 1)
rotat_l _ = error "rotat no werk"

rotat_r = rotat_l . rotat_l . rotat_l

ant :: (Socket, SockAddr) -> IO ()
ant (s, _) = do
    ant_ 500 500 (1, 0)
    where ant_ x y (dx, dy) = do
            let (nx, ny) = (x + dx, y + dy)
            here_px <- read_pixel_block s [(nx, ny)] >>= pure . head
            let px_black = (>) 370 $ (\(r, b, g) -> r + b + g) here_px

            let (new_px, new_dir) = if px_black
                    then ([(nx, ny, 255, 255, 255)], rotat_l (dx, dy))
                    else ([(nx, ny, 0, 0, 0)], rotat_r (dx, dy))

            write_image s new_px
            ant_ nx ny new_dir

main = do
    args <- getArgs

    f <- case (listToMaybe args) of
        Nothing -> pure $ paddle False paddle_im
        Just "gay" -> pure $ paddle True paddle_im
        Just "border" -> pure $ conf_im border
        Just "clear" -> pure $ conf_im clear
        Just "ant" -> pure ant
        Just s -> read_im s >>= pure . paddle False

    connect "pixelflut.uwu.industries" "1234" f
