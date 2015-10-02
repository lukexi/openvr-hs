module Framebuffer where

import Graphics.GL.Pal
import Control.Monad.Trans

withFramebuffer :: MonadIO m => GLuint -> m a -> m ()
withFramebuffer framebuffer action = do
  glBindFramebuffer GL_FRAMEBUFFER framebuffer
  _ <- action
  glBindFramebuffer GL_FRAMEBUFFER 0


-- | Create and configure the texture to use for our framebuffer
createFramebufferTexture :: GLsizei -> GLsizei -> IO GLuint
createFramebufferTexture sizeX sizeY = do
  texID <- overPtr (glGenTextures 1)
  
  glBindTexture   GL_TEXTURE_2D texID
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_BORDER
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_BORDER
  glTexStorage2D  GL_TEXTURE_2D 1 GL_RGBA8 sizeX sizeY
  glBindTexture   GL_TEXTURE_2D 0
  
  return texID

-- | Create the framebuffer we'll render into and pass to the Oculus SDK
createFramebuffer :: GLsizei -> GLsizei -> IO (GLuint, GLuint)
createFramebuffer sizeX sizeY = do
  putStrLn $"Creating framebuffer of size" ++ show (sizeX, sizeY)
  framebufferTexture <- createFramebufferTexture sizeX sizeY

  framebuffer <- overPtr (glGenFramebuffers 1)

  -- Attach the eye texture as the color buffer
  glBindFramebuffer GL_FRAMEBUFFER framebuffer
  glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D framebufferTexture 0

  -- Generate a render buffer for depth
  renderbuffer <- overPtr (glGenRenderbuffers 1)

  -- Configure the depth buffer dimensions to match the eye texture
  glBindRenderbuffer GL_RENDERBUFFER renderbuffer
  glRenderbufferStorage GL_RENDERBUFFER GL_DEPTH_COMPONENT16 sizeX sizeY
  glBindRenderbuffer GL_RENDERBUFFER 0

  -- Attach the render buffer as the depth target
  glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER renderbuffer

  -- Unbind the framebuffer
  glBindFramebuffer GL_FRAMEBUFFER 0

  return (framebuffer, framebufferTexture)
