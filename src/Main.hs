module Main where

import RedPill.Window.Win32.Window (createWindow)
import RedPill.Window.WindowStyle

main :: IO ()
main = do
  createWindow "RedPill" 400 300 WindowStyleWindow

