module Main where

import Network.Simple.TCP

import FlutFmt
--import FlutIO (write_image, read_pixel_block)
import FlutIO
import Rainbow (color)

plot_fn f = [(x, floor y, r, g, b) |
    x <- [0..1280],
    let n_x = fromIntegral x / 1280,
    let y = 360 + f n_x * (-300),
    let (r, g, b) = (255, 255, 255)]
    --let (r, g, b) = color x $ floor y]

sine_plot = plot_fn (sin . (*6.283))

sine_plots = map (\i -> plot_fn (sin . (\x -> x * 15 + i * 5))) [0..30]

draw :: (Socket, SockAddr) -> IO ()
draw (s, _) = do
    mapM (write_image s) sine_plots
    pure ()

test :: (Socket, SockAddr) -> IO ()
test (s, _) = do
    pixels <- read_pixel_block s [(0, 0), (100, 100), (600, 100)]
    mapM print pixels
    pure ()

main = do
    connect "pixelflut.uwu.industries" "1234" test
    putStrLn "Done :)"
