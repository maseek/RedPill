{-# LANGUAGE ForeignFunctionInterface #-}

module RedPill.Window.Win32.Window where

import System.Win32.DLL (getModuleHandle)
import System.Win32.Types (WPARAM, LPARAM, LRESULT, DWORD)
import Graphics.Win32.Window as WindowWin32
    (mkClassName, registerClass, cS_VREDRAW, cS_HREDRAW, createWindow, wS_THICKFRAME, wS_CAPTION, wS_SYSMENU,
    wS_MINIMIZEBOX, wS_MAXIMIZEBOX, wS_VISIBLE,
    showWindow, sW_SHOWNORMAL, updateWindow, allocaMessage, unregisterClass, getMessage,
    translateMessage, dispatchMessage, defWindowProc, sendMessage, allocaPAINTSTRUCT, beginPaint,
    endPaint, LPMSG, adjustWindowRect)
import Graphics.Win32.Misc (loadCursor, iDC_ARROW)
import Graphics.Win32.GDI.Brush (getStockBrush, bLACK_BRUSH)
import Graphics.Win32.GDI.Types (HWND)
import Graphics.Win32.Message
    (WindowMessage, wM_KEYDOWN, wM_LBUTTONDOWN, wM_DESTROY, wM_PAINT, wM_CLOSE,
    wM_SYSKEYDOWN)
import RedPill.Window.WindowStyle
import Data.Int (Int32)
import Data.Maybe
import Control.Monad
import RedPill.Window.VirtualKey

foreign import stdcall "PostQuitMessage" postQuitMessage :: Int32 -> IO ()

createWindow :: String -> Int -> Int -> WindowStyle -> IO ()
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
        windowSize = tryAdjustWindowRect windowWidth windowHeight windowStyle realWindowStyle
    hWnd <- WindowWin32.createWindow 
      className -- ClassName
      title -- Title
      realWindowStyle -- WindowStyle
      (Just 0) -- x
      (Just 0) -- y
      (Just $ fst windowSize) -- w
      (Just $ snd windowSize) -- h
      Nothing -- Maybe HWND
      Nothing -- Maybe HMENU 
      hInst -- HINSTANCE
      wndProc

    _ <- showWindow hWnd sW_SHOWNORMAL
    updateWindow hWnd
    allocaMessage pump

    unregisterClass className hInst

translateWindowStyle :: WindowStyle -> DWORD
translateWindowStyle windowStyle =
  if windowStyle == windowStyleWindow
    then wS_VISIBLE + wS_CAPTION + wS_MINIMIZEBOX + wS_THICKFRAME + wS_MAXIMIZEBOX + wS_SYSMENU
    else wS_VISIBLE

tryAdjustWindowRect :: Int -> Int -> WindowStyle -> DWORD -> (Int, Int)
tryAdjustWindowRect windowWidth windowHeight windowStyle realWindowStyle = let
  rect = (0, 0, fromIntegral windowWidth :: Int32, fromIntegral windowHeight :: Int32)
  newRect = adjustWindowRect rect realWindowStyle False in
  if windowStyle == windowStyleWindow
    then (windowWidth, windowHeight)
    else (windowWidth, windowHeight)

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
  | wm == wM_SYSKEYDOWN || wm == wM_KEYDOWN = inputKeyEvent wp >> return 0
  | otherwise = defWindowProc (Just hWnd) wm wp lp
  where
    doFinish = sendMessage hWnd wM_CLOSE 1 0 >> return 0
    onPaint = allocaPAINTSTRUCT $ \ lpps -> do
      hdc <- beginPaint hWnd lpps
      endPaint hWnd lpps
      return 0
    inputKeyEvent vk = print vk
