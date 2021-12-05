module FlutImage (read_im, move_im, recolor_im, render_text) where

import Data.Maybe

import Codec.Picture (readImageWithMetadata, convertRGBA8, pixelAt, PixelRGBA8(PixelRGBA8))
import Codec.Picture.Metadata as M (Metadatas, Keys(Width, Height), lookup)

import Graphics.Text.TrueType (loadFontFile, Font)
import Graphics.Rasterific
import Graphics.Rasterific.Texture

fromRight :: Either a b -> b
fromRight (Left _) = error "fromRight called on Left"
fromRight (Right a) = a

get_dims :: M.Metadatas -> Maybe (Int, Int)
get_dims meta = do
    w <- M.lookup M.Width meta
    h <- M.lookup M.Height meta
    pure (fromIntegral w, fromIntegral h)

read_pixels im w h = map (\(x, y) -> (x, y, pixelAt im x y)) [(x, y) | x <- [0..w - 1], y <- [0..h - 1]]

im8_fmt im8 w h = filter_alpha (expand_color $ read_pixels im8 w h)
    where fi = fromIntegral
          filter_alpha = mapMaybe (\(x, y, r, g, b, a) -> if (a > 127) then Just (x, y, fi r, fi g, fi b) else Nothing)
          expand_color = map (\(x, y, PixelRGBA8 r g b a) -> (x, y, r, g, b, a))

read_im :: FilePath -> IO [(Int, Int, Int, Int, Int)]
read_im path = do
    im_ <- readImageWithMetadata path
    let (im, meta) = fromRight im_
    let im8 = convertRGBA8 im
    let (w, h) = fromJust $ get_dims meta
    pure $ im8_fmt im8 w h

render_text f s = do
    ef <- loadFontFile f
    let font = fromRight ef

    let (w, h) = (1000, 45)

    let im = renderDrawing w h transp. withTexture (uniformTexture white) $
            printTextAt font (PointSize 15) (V2 20 30) s

    pure $ im8_fmt im w h
    where white = PixelRGBA8 255 255 255 255
          transp = PixelRGBA8 0 0 0 0

move_im dx dy = map (\(x, y, r, g, b) -> (x + dx, y + dy, r, g, b))
recolor_im (r, g, b) = map (\(x, y, _, _, _) -> (x, y, r, g, b))
