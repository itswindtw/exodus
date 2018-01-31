{-# LANGUAGE ForeignFunctionInterface #-}
module CairoFFI --(
--  patternGetSurface
--) 
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified System.IO
import Foreign hiding (void)
import Foreign.C
import Foreign.StablePtr

import Control.Monad (void)
import System.FilePath (takeExtension)

import Graphics.Rendering.Cairo.Types

foreign import ccall "wrapper"
    wrap :: (StablePtr a -> IO ()) -> IO (FunPtr (StablePtr a -> IO ()))

foreign import ccall "cairo.h cairo_surface_set_mime_data"
    cairoSurfaceSetMIMEData :: Ptr Surface -> CString
                            -> Ptr a -> CULong
                            -> FunPtr (StablePtr a -> IO ()) 
                            -> StablePtr b
                            -> IO CUInt

surfaceSetMIMEData :: Surface -> String -> IO ()
surfaceSetMIMEData surface path = do
  fsize <- fromInteger <$> System.IO.withFile path System.IO.ReadMode System.IO.hFileSize
  dataBytes <- BS.readFile path
  dataStablePtr <- newStablePtr dataBytes
   
  destructFunc <- wrap (\ptr -> freeStablePtr ptr)

  withSurface surface $ \surfacePtr ->
    withCString mimeType $ \mtype ->
      BS.unsafeUseAsCString dataBytes $ \cs ->
        void $ cairoSurfaceSetMIMEData surfacePtr mtype (castPtr cs) (fromIntegral fsize) destructFunc dataStablePtr

  where mimeType = case takeExtension path of
                     ".png" -> "image/png"
                     _ -> "image/jpeg"
                        
    
