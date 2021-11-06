module Rainbow where

freq = 0.1
color :: Int -> Int -> (Int, Int, Int)
color x y = (r n, g n, b n)
    where n = x + y
          sin_arg off = freq * (fromIntegral n) + pi / 1.5 * off
          single_color off n = floor $ sin (sin_arg off) * 127 + 128
          r = single_color 0
          g = single_color 1
          b = single_color 2
