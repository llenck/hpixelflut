module Main where

import System.IO
import System.Environment
import Control.Monad
import Data.Maybe

import Network.Simple.TCP
import Graphics.Text.TrueType(loadFontFile)
import Graphics.Rasterific
import Graphics.Rasterific.Texture

import FlutFmt
import FlutIO (conf_im, write_image, read_pixel_block)
import FlutImage (read_im, move_im, recolor_im)
import Rainbow

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

            write_image s $ recolor_im (0, 0, 0) im
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
        Just "im" -> case (listToMaybe $ drop 1 args) of
                Nothing -> error "command \"im\" needs an argument :("
                Just s -> read_im s >>= pure . paddle False

    connect "pixelflut.uwu.industries" "1234" f
