module FlutFmt where

import Text.Printf (printf)

fmt_px :: Int -> Int -> Int -> Int -> Int -> String
fmt_px = printf "PX %d %d %02X%02X%02X\n"

fmt_rq :: Int -> Int -> String
fmt_rq = printf "PX %d %d\n"
