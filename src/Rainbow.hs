module Rainbow where

freq = 0.1
rainbow :: Int -> (Int, Int, Int)
rainbow x = (r x, g x, b x)
    where sin_arg off = freq * (fromIntegral x) + pi / 1.5 * off
          single_color off n = floor $ sin (sin_arg off) * 127 + 128
          r = single_color 0
          g = single_color 1
          b = single_color 2
