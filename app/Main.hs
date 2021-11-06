module Main where

import System.IO

import Network.Simple.TCP

import FlutFmt
import FlutIO (pixelflut, write_image, read_pixel_block)
import Rainbow (color)

plot_fn f = [(x, floor y, r, g, b) |
    x <- [0..1280],
    let n_x = fromIntegral x / 1280,
    let y = 360 + f n_x * (-360),
    --let (r, g, b) = (255, 255, 255)]
    let (r, g, b) = color x $ floor y]

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

paddle :: (Socket, SockAddr) -> IO ()
paddle (s, _) = do
    hSetBuffering stdin NoBuffering
    paddle_ s 310 0
    pure ()
    where paddle_ s y counter = do
            let new_counter = counter + 1
            let (r, g, b) = color new_counter 0
            x <- getChar
            let new_y = y + case x of
                    'w' -> -10
                    's' -> 10
                    _   -> 0

            write_image s $ block 30 new_y 50 200 r g b
            write_image s $ block 30 (new_y - 10) 50 10 0 0 0
            write_image s $ block 30 (new_y + 200) 50 10 0 0 0
            paddle_ s new_y new_counter

main = do
    --pixelflut "pixelflut.uwu.industries" "1234" $ [copy 0 0 400 100 0 500] ++ sine_plots
    connect "pixelflut.uwu.industries" "1234" paddle
    putStrLn "Done :)"
