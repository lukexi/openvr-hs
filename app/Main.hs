module Main where

import Graphics.VR.OpenVR
import Graphics.GL.Pal
import Graphics.UI.GLFW.Pal

main :: IO ()
main = do
  putStrLn "Starting OpenVR"
  mSystem <- initOpenVR

  case mSystem of
    Nothing -> putStrLn "Couldn't create OpenVR system :*("
    Just system -> do
      (w,h) <- getRenderTargetSize system
      print (w,h)
      putStrLn $ "Got system: " ++ show system
  putStrLn "Done!"
