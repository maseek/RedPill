{-# LANGUAGE ForeignFunctionInterface #-}

module RedPill.Window.Win32.Window (RedPill.Window.Win32.Window.createWindow) where

import System.Win32.DLL (getModuleHandle)
import System.Win32.Types (WPARAM, LPARAM, LRESULT, DWORD)
import Graphics.Win32.Window as WinWindow32
    (mkClassName, registerClass, cS_VREDRAW, cS_HREDRAW, createWindow, wS_THICKFRAME, wS_CAPTION, wS_SYSMENU,
    wS_MINIMIZEBOX, wS_MAXIMIZEBOX, wS_VISIBLE,
    showWindow, sW_SHOWNORMAL, updateWindow, allocaMessage, unregisterClass, getMessage,
    translateMessage, dispatchMessage, defWindowProc, sendMessage, allocaPAINTSTRUCT, beginPaint,
    endPaint, LPMSG, adjustWindowRect)
import Graphics.Win32.Misc (loadCursor, iDC_ARROW)
import Graphics.Win32.GDI.Brush (getStockBrush, bLACK_BRUSH)
import Graphics.Win32.GDI.Types (HWND, RECT)
import Graphics.Win32.Message
    (WindowMessage, wM_KEYDOWN, wM_LBUTTONDOWN, wM_DESTROY, wM_PAINT, wM_CLOSE,
    wM_SYSKEYDOWN)
import RedPill.Graphics.D3D11.Renderer
import RedPill.Window.WindowStyle
import Data.Int (Int32)
import Data.Maybe
import Control.Monad

foreign import stdcall "PostQuitMessage" postQuitMessage :: Int32 -> IO ()

createWindow :: String -> Int32 -> Int32 -> WindowStyle -> IO ()
createWindow title windowWidth windowHeight windowStyle = do
  let className =  mkClassName "RedPill Window"
  hInst <- getModuleHandle Nothing
  whiteBrush <- getStockBrush bLACK_BRUSH
  curArrow <- loadCursor Nothing iDC_ARROW
  mAtom <- registerClass
    (cS_VREDRAW + cS_HREDRAW, -- ClassStyle
    hInst, -- HINSTANCE
    Nothing, -- Maybe HICON
    Just curArrow, -- Maybe HCURSOR
    Just whiteBrush, -- Maybe HBRUSH
    Nothing, -- Maybe LPCTSTR
    className) -- ClassName
    
  when (isJust mAtom) $ do
    let realWindowStyle = translateWindowStyle windowStyle
    windowSize <- tryAdjustWindowRect windowWidth windowHeight windowStyle realWindowStyle
    hWnd <- WinWindow32.createWindow 
      className -- ClassName
      title -- Title
      realWindowStyle -- WindowStyle
      (Just 0) -- x
      (Just 0) -- y
      (Just (fromIntegral (fst windowSize) :: Int)) -- w
      (Just (fromIntegral (snd windowSize) :: Int)) -- h
      Nothing -- Maybe HWND
      Nothing -- Maybe HMENU 
      hInst -- HINSTANCE
      wndProc
    
    _ <- initRenderer hWnd windowStyle windowWidth windowHeight
    -- _ <- showWindow hWnd sW_SHOWNORMAL -- only in fullscreen
    updateWindow hWnd
    allocaMessage pump

    unregisterClass className hInst

translateWindowStyle :: WindowStyle -> DWORD
translateWindowStyle WindowStyleWindow = wS_VISIBLE + wS_CAPTION + wS_MINIMIZEBOX + wS_THICKFRAME + wS_MAXIMIZEBOX + wS_SYSMENU
translateWindowStyle _ = wS_VISIBLE

tryAdjustWindowRect :: Int32 -> Int32 -> WindowStyle -> DWORD -> IO (Int32, Int32)
tryAdjustWindowRect windowWidth windowHeight WindowStyleWindow realWindowStyle = do
  newRect <- adjustWindowRect (0, 0, windowWidth, windowHeight) realWindowStyle False
  return (rectToWindowSize newRect)
tryAdjustWindowRect windowWidth windowHeight _ _ = return (windowWidth, windowHeight)

rectToWindowSize :: RECT -> (Int32, Int32)
rectToWindowSize rect = (sel3 rect - sel1 rect, sel4 rect - sel2 rect)
                   where sel1 (s,_,_,_) = s
                         sel2 (_,s,_,_) = s
                         sel3 (_,_,s,_) = s
                         sel4 (_,_,_,s) = s

pump :: LPMSG -> IO ()
pump lpmsg = do
  fContinue <- getMessage lpmsg Nothing
  when fContinue $ do
    _ <- translateMessage lpmsg
    _ <-dispatchMessage lpmsg
    pump lpmsg

wndProc :: HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
wndProc hWnd wm wp lp
  | wm == wM_LBUTTONDOWN = doFinish
  | wm == wM_DESTROY = postQuitMessage 0 >> return 0
  | wm == wM_PAINT = onPaint
  | wm == wM_SYSKEYDOWN || wm == wM_KEYDOWN = print wp >> return 0
  | otherwise = defWindowProc (Just hWnd) wm wp lp
  where
    doFinish = sendMessage hWnd wM_CLOSE 1 0 >> return 0
    onPaint = allocaPAINTSTRUCT $ \ lpps -> do
      _ <- beginPaint hWnd lpps
      endPaint hWnd lpps
      return 0
