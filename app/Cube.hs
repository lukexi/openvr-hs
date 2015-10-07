{-# LANGUAGE TemplateHaskell #-}
module Cube where
import Graphics.GL.Pal
import Linear.Extra
import Control.Lens.Extra
data Cube = Cube
  { _cubMatrix :: M44 GLfloat
  , _cubColor  :: V4 GLfloat
  }
makeLenses ''Cube