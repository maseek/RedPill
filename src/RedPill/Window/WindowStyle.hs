module RedPill.Window.WindowStyle where

import Data.Word (Word8)

type WindowStyle = Word8

windowStyleWindow :: WindowStyle
windowStyleWindow = 0

windowStyleFullscreen :: WindowStyle
windowStyleFullscreen = 1
