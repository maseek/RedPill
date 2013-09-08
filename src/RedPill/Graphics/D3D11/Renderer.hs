module RedPill.Graphics.D3D11.Renderer (initRenderer) where

import RedPill.Window.WindowStyle
import Graphics.Win32.GDI.Types (HWND)
import Data.Int (Int32)

initRenderer :: HWND -> WindowStyle -> Int32 -> Int32 -> IO ()
initRenderer hWnd WindowStyleWindow width height = initWindowRenderer hWnd width height
initRenderer hWnd WindowStyleFullscreen width height = initFullscreenRenderer hWnd width height
  
initWindowRenderer hWnd width height = undefined

initFullscreenRenderer hWnd width height =
  undefined
  