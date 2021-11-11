module FlutImage (read_im, move_im, recolor_im) where

import Data.Maybe

import Codec.Picture (readImageWithMetadata, convertRGBA8, pixelAt, PixelRGBA8(PixelRGBA8))
import Codec.Picture.Metadata as M (Metadatas, Keys(Width, Height), lookup)

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
