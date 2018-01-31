{-# LANGUAGE ScopedTypeVariables #-}
module PageComposer where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, ord)
import Data.List (isPrefixOf, intercalate, stripPrefix)
import Data.Maybe (maybe, fromMaybe)
import Control.Applicative (liftA2)
import Control.Monad (void)
import Control.Monad.State
import Control.Exception (try, SomeException)
import Numeric (readHex)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.IO.Temp (withSystemTempFile, openTempFile)
import System.IO (hClose)
import System.Directory (renameFile, copyFile, removeFile, getTemporaryDirectory, createDirectoryIfMissing)
import System.Process (readProcess, readProcessWithExitCode)
import System.FilePath (takeExtension, takeDirectory)

import qualified Graphics.UI.Gtk.Cairo as Cairo (setSourcePixbuf)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.Matrix as Matrix
import qualified Graphics.UI.Gtk.Gdk.Pixbuf as Pixbuf
import qualified Graphics.Rendering.Pango as Pango

import PCD
import CairoFFI

import Debug.Trace

runPCD :: PCD -> Maybe String -> IO ()
runPCD pcd overridedPath = withSystemTempFile "pcd.pdf" $ \tempf handle -> do
  runPDF tempf pcd
  hClose handle
  
  case pcdOutput pcd of
    PCD2PDF realf -> do
      createDirectoryIfMissing True (takeDirectory . realOutputPath $ realf)
      copyFile tempf $ realOutputPath realf
    PCD2JPG realf (JDC dpi quality) -> do
      createDirectoryIfMissing True (takeDirectory . realOutputPath $ realf)
      let realPath = realOutputPath realf

      void $ readProcess "pdftocairo" [ "-r", show dpi, "-singlefile", "-jpeg"
                                      , tempf, realPath ] []

      void $ readProcess "gm" [ "mogrify", "-quality", show (round $ quality * 100), realPath ++ ".jpg" ] []

      renameFile (realPath ++ ".jpg") (realPath)
    PCD2JPG realf (JWHC width height quality) -> do
      createDirectoryIfMissing True (takeDirectory . realOutputPath $ realf)
      let targetDPI = max (width * 72 / pcdWidth pcd) (height * 72 / pcdHeight pcd)
          realPath = realOutputPath realf

      void $ readProcess "pdftocairo" [ "-r", show targetDPI, "-singlefile", "-jpeg"
                                      , tempf, realPath ] []

      void $ readProcess "gm" [ "mogrify", "-quality", show (round $ quality * 100), realPath ++ ".jpg" ] []

      renameFile (realPath ++ ".jpg") (realPath)
    PCD2PNG realf (PDS dpi scale) -> do
      createDirectoryIfMissing True (takeDirectory . realOutputPath $ realf)
      let width = pcdWidth pcd * (fromIntegral dpi / 72) * scale
          height = pcdHeight pcd * (fromIntegral dpi / 72) * scale
          targetDPI = max (width * 72 / pcdWidth pcd) (height * 72 / pcdHeight pcd)
          realPath = realOutputPath realf
  
      void $ readProcess "pdftocairo" [ "-r", show targetDPI, "-singlefile", "-png", "-transp"
                                    , tempf, realPath ] []

      renameFile (realPath ++ ".png") (realPath)
    PCD2PNG realf (PWH width height) -> do
      createDirectoryIfMissing True (takeDirectory . realOutputPath $ realf)
      let targetDPI = max (width * 72 / pcdWidth pcd) (height * 72 / pcdHeight pcd)
          realPath = realOutputPath realf

      void $ readProcess "pdftocairo" [ "-r", show targetDPI, "-singlefile", "-png", "-transp"
                                      , tempf, realPath ] []

      renameFile (realPath ++ ".png") (realPath)
                                    
  where runPDF fpath = Cairo.withPDFSurface <$> (pure fpath) <*> pcdWidth <*> pcdHeight
          <*> flip Cairo.renderWith . doRunPCD . ((,,) <$> pcdWidth <*> pcdHeight <*> pcdContents)
        realOutputPath realf = fromMaybe realf overridedPath

doRunPCD :: (Double, Double, [PCDContent]) -> Cairo.Render ()
doRunPCD (w, h, cs) = do
  flip evalStateT (initPCDOptions, Map.empty) $ mapM_ (runPCDContent (w, h)) cs

runPCDContent :: (Double, Double) 
              -> PCDContent
              -> StateT (PCDOptions, Map.Map PCDColor PCDColor) Cairo.Render ()

runPCDContent (pcdw, pcdh) (SimpleColor color x y w h) = do
  (opts, colorMap) <- get 

  (RGBA r g b a) <- case color of
    CMYK c m y k a -> do
      (rgba, colorMap') <- liftIO $ runStateT (cmykToRGBA (CMYK c m y k a)) colorMap
      put (opts, colorMap')
      return rgba
    _ -> return color

  lift $ do
    Cairo.setSourceRGBA r g b a
    Cairo.rectangle x (pcdh-y-h) w h
    Cairo.fill

runPCDContent pcdwh (SimpleImage path x y w h) =
  runPCDContent pcdwh (Image path "0" "0" "x1" "x1" x y w h)

runPCDContent pcdwh (SimpleImageCompress path compress x y w h) =
  runPCDContent pcdwh (ImageCompress path compress "0" "0" "x1" "x1" x y w h)

runPCDContent pcdwh (ImageCompress path compress fromX fromY fromW fromH x y w h) = do
  case takeExtension path of
    ext | ext == ".jpg" || ext == ".jpeg" -> do
      tempf <- lift $ Cairo.liftIO $ do
        tempdir <- getTemporaryDirectory
        (tempf, handle) <- openTempFile tempdir ("image_compress" ++ ext)
        hClose handle
        readProcess "gm" [ "convert", "-quality", show normalizedCompress
                         , "-sampling-factor", "4:2:2"
                         , "-interlace", "Plane"
                         , path, tempf ] []
        return tempf

      runWithImage tempf

      lift $ Cairo.liftIO $ removeFile tempf
    _ -> runWithImage path

  where runWithImage path' = runPCDContent pcdwh (Image path' fromX fromY fromW fromH x y w h)
        scaledCompress = 100 * compress
        normalizedCompress :: Integer
        normalizedCompress
          | scaledCompress <= 0 = 0
          | scaledCompress <= 50 = ceiling $ 30 + compress
          | scaledCompress <= 70 = 80 + (ceiling $ (compress-50) / 2)
          | scaledCompress < 100 = 90 + (ceiling $ (compress-70) / 5)
          | scaledCompress >= 100 = 100

runPCDContent (pcdw, pcdh) (Image path rawFromX rawFromY rawFromW rawFromH x y w h) = do
  (opts, colorMap) <- get

  lift $ do
    let rotation = fromMaybe "" (pcdOptContentRotation opts)
        isRotated = rotation == "clockwise" || rotation == "counter-clockwise"
        isScale = or <$> sequence (isPrefixOf <$> ["x", "X", "*"])
        convertWith m s = if isScale s then m * (read $ drop 1 s) else read s

    converted <- Cairo.liftIO $ try $ do
      -- colorspace conversion
      tempdir <- getTemporaryDirectory
      (tempf, handle) <- openTempFile tempdir ("image" ++ takeExtension path)
      hClose handle

      infoStr <- readProcess "gm" [ "identify", "-format", "%Q %w %h", path ] []
      -- get quality, pw, ph
      (quality, pw, ph) <- maybe (fail "Unable to identity input image")
            (return)
            (case words infoStr of
              [rawQuality, rawPW, rawPH] -> Just (rawQuality, read rawPW :: Double,  read rawPH :: Double)
              _ -> Nothing)

      (code, _, _) <- readProcessWithExitCode "gm" [ "convert", path, "icc:" ++ tempf ++ ".icc" ] []
      case code of
        ExitSuccess -> do
          readProcess "gm" [ "convert", "-profile", "res/sRGB.icc", path, "-quality", quality, tempf] []
          removeFile $ tempf ++ ".icc"
        _ -> copyFile path tempf

      -- resize if necessary 
      let ratio :: Maybe Double
          ratio = case pcdOptMaxDPI opts of
                    Nothing -> Nothing
                    Just dpi -> do
                      let realPW = if isRotated then ph else pw
                          realPH = if isRotated then pw else ph
                          fromW = convertWith realPW rawFromW
                          fromH = convertWith realPH rawFromH
                          maxDPIW = dpi / 72 * w
                          maxDPIH = dpi / 72 * h
                          ratioW = maxDPIW / fromW
                          ratioH = maxDPIH / fromH
                          ratio = min ratioW ratioH
                    
                      if ratio < 1 then return ratio else Nothing

      maybe (pure ()) (\r -> 
        let scaledW = r * pw
            scaledH = r * ph
        in  void $ readProcess "gm" [ "mogrify", "-resize", show scaledW ++ "x" ++ show scaledH ++ "!", tempf ] []
        ) ratio

      -- createSurface from ext
      surface <- case takeExtension path of
                   ".png" -> Cairo.imageSurfaceCreateFromPNG tempf
                   _ -> do
                     surface <- Cairo.createImageSurface Cairo.FormatRGB24 (round pw) (round ph)
                     surfaceSetMIMEData surface tempf
                     return surface
      return (surface, pw, ph, ratio)

    case converted of
      Left (e :: SomeException) -> do
        Cairo.liftIO $ putStrLn $ show e
        Cairo.setSourceRGBA 1.0 0 0 1.0
      Right (surface, pw, ph, scaled) -> preserveMatrix $ do

        Cairo.translate x (pcdh-y-h)

        -- radius
        case pcdOptRadius opts of
          Just r -> do
            Cairo.arc (w-r) (r) r (negate (pi/2)) 0
            Cairo.arc (w-r) (h-r) r 0 (pi/2)
            Cairo.arc (r) (h-r) r (pi/2) (pi)
            Cairo.arc (r) (r) r (pi) (pi*3/2)
            Cairo.closePath
            Cairo.clip
          _ -> pure ()
       
        -- parse fromX, fromY, fromW, fromH

        let realPW = if isRotated then ph else pw
            realPH = if isRotated then pw else ph
            fromX = convertWith realPW rawFromX
            fromY = convertWith realPH rawFromY
            fromW = convertWith realPW rawFromW
            fromH = convertWith realPH rawFromH
            croppedW = min fromW (realPW-fromX)
            croppedH = min fromH (realPH-fromY)

        -- handle crop
        Cairo.scale (w/croppedW) (h/croppedH)
        Cairo.rectangle 0 0 croppedW croppedH
        Cairo.translate (negate fromX) (negate fromY)
        -- handle rotation
        case pcdOptContentRotation opts of
          Just "clockwise" -> do
            Cairo.translate ph 0
            Cairo.rotate (pi/2)
          Just "counter-clockwise" -> do
            Cairo.translate 0 pw
            Cairo.rotate (negate (pi/2))
          Just "half" -> do
            Cairo.translate pw ph
            Cairo.rotate pi
          Nothing -> pure ()

        case scaled of 
          Nothing -> pure ()
          Just ratio -> Cairo.scale (1/ratio) (1/ratio)

        Cairo.setSourceSurface surface 0 0

        Cairo.clip
        Cairo.paint
        Cairo.resetClip

        Cairo.surfaceFinish surface

  put (unmarkImageOpts opts, colorMap)

runPCDContent (pcdw, pcdh) (SimpleText x y str) = do
  lift $ preserveMatrix $ do
    let markupAttrs = buildMarkupAttrs initPCDOptions

    (layout, rx, ry, rw, rh) <- Cairo.liftIO $ do
      fontMap <- Pango.cairoFontMapGetDefault
      Pango.cairoFontMapSetResolution fontMap 72.0
      ctx <- Pango.cairoCreateContext (Just fontMap)
      layout <- Pango.layoutEmpty ctx

      (_ :: String) <- Pango.layoutSetMarkup layout (markupText markupAttrs str) 
      (_, Pango.PangoRectangle rx ry rw rh) <- Pango.layoutGetExtents layout
      return (layout, rx, ry, rw, rh)

    Cairo.translate (x+rx) (pcdh-y-ry-rh)

    Cairo.setSourceRGBA 0.0 0.0 0.0 1.0

    Pango.updateLayout layout
    Pango.showLayout layout

runPCDContent (pcdw, pcdh) (Text checkSize vtext x y w h str) = do
  (opts, colorMap) <- get
  
  let rotation = fromMaybe "" (pcdOptContentRotation opts)
      isRotated = rotation == "clockwise" || vtext

  rgba <- case pcdOptColor opts of
            Nothing -> return $ RGBA 0.0 0.0 0.0 1.0
            Just rgba@(RGBA _ _ _ _) -> return rgba
            Just cmyk@(CMYK c m y k a) -> do
              (rgba, colorMap') <- liftIO $ runStateT (cmykToRGBA (CMYK c m y k a)) colorMap
              put (opts, colorMap')
              return rgba

  lift . preserveMatrix $ do
    let markupAttrs = buildMarkupAttrs opts
        realW = if isRotated then h else w
        realH = if isRotated then w else h

    (layout, ix, iy, iw, ih, rx, ry, rw, rh, baseline, offset) <- Cairo.liftIO $ do
      fontMap <- Pango.cairoFontMapGetDefault
      Pango.cairoFontMapSetResolution fontMap 72.0
      ctx <- Pango.cairoCreateContext (Just fontMap)
      Pango.contextSetTextGravity ctx (if vtext then Pango.PangoGravityAuto else Pango.PangoGravitySouth)

      layout <- Pango.layoutEmpty ctx
      Pango.layoutSetWidth layout (Just realW)
      maybe (pure ()) (Pango.layoutSetSpacing layout) (pcdOptLineSpacing opts)
      maybe (pure ()) (Pango.layoutSetAlignment layout) (pcdOptTextAlign opts)
      (_ :: String) <- Pango.layoutSetMarkup layout (markupText markupAttrs str) 
      (Pango.PangoRectangle ix iy iw ih, Pango.PangoRectangle rx ry rw rh) <- Pango.layoutGetExtents layout

      it <- Pango.layoutGetIter layout
      proceedToLastLine it
      baseline <- Pango.layoutIterGetBaseline it
      (Pango.PangoRectangle lrx lry lrw lrh, Pango.PangoRectangle lix liy liw lih) <- Pango.layoutIterGetLineExtents it
 
      --traceIO $ show checkSize ++ show vtext ++ ", ix: " ++ show ix ++ ", iy: " ++ show iy ++ ", iw: " ++ show iw ++ ", ih: " ++ show ih
      --traceIO $ show checkSize ++ show vtext ++ ", rx: " ++ show rx ++ ", ry: " ++ show ry ++ ", rw: " ++ show rw ++ ", rh: " ++ show rh
      --traceIO $ show checkSize ++ show vtext ++ ", baseline: " ++ show baseline 
      if checkSize && (rw > realW || rh > realH) then exitWith (ExitFailure 1) else pure ()

      return (layout, ix, iy, iw, ih, rx, ry, rw, rh, baseline, (rh+ry-baseline)/2)

    if isRotated then do
      --Cairo.translate (rh+ry) (pcdh-y-realW)
      Cairo.translate (x-offset) (pcdh-y-realW)
      case pcdOptTextVerticalAlign opts of
        Just "top" -> Cairo.translate (realH+baseline-ih) 0
        Just m | m == "middle" || m == "center" -> Cairo.translate ((realH+baseline-ih+rh+ry)/2) 0
        Just "bottom" -> Cairo.translate (rh+ry) 0
        _ -> Cairo.translate (realH+baseline-ih) 0

      Cairo.rotate (pi/2)

    else do
      Cairo.translate (x) (pcdh-y-realH+offset)
      case pcdOptTextVerticalAlign opts of
        Just "top" -> Cairo.translate 0 (negate iy)
        Just m | m == "middle" || m == "center" -> Cairo.translate 0 (((negate iy)+(realH-rh-ry))/2)
        Just "bottom" -> Cairo.translate 0 (realH-rh-ry)
        Just s | isPrefixOf "bottom-baseline:" s -> do
          let baselineOffset = read $ fromMaybe "0" (stripPrefix "bottom-baseline:" s)
          Cairo.translate 0 (realH-baseline-baselineOffset-offset)
          
        _ -> Cairo.translate 0 (negate iy)

    case rgba of
      (RGBA r g b a) -> Cairo.setSourceRGBA r g b a

    Pango.updateLayout layout
    
    case pcdOptLineHeight opts of
      Nothing -> Pango.showLayout layout
      Just lh -> do
        (iter, baseline, rh, lineSpacing) <- Cairo.liftIO $ do
          it <- Pango.layoutGetIter layout
          baseline <- Pango.layoutIterGetBaseline it
          lineSpacing <- Pango.layoutGetSpacing layout
          (_, Pango.PangoRectangle _ _ _ rh) <- Pango.layoutIterGetLineExtents it
          return (it, baseline, rh, lineSpacing)

        Cairo.translate 0 (lh-rh+baseline-offset+iy)
        showLayoutLines iter (lh+lineSpacing) -- +offset)

  (_, colorMap') <- get
  put (unmarkTextOpts opts, colorMap') 

runPCDContent _ (Option k v) = do
  (opts, colorMap) <- get
  case Map.lookup k pcdOptionsMap of
    Nothing -> return ()
    Just f -> put (f opts v, colorMap)

runPCDContent _ (ColorOption color) = do
  (opts, colorMap) <- get
  put (opts { pcdOptColor = color }, colorMap)

showLayoutLines :: Pango.LayoutIter -> Double -> Cairo.Render ()
showLayoutLines iter offset = do
  (line, rx, ry, rw, rh) <- Cairo.liftIO $ do
    (_, Pango.PangoRectangle rx ry rw rh) <- Pango.layoutIterGetLineExtents iter
    line <- Pango.layoutIterGetLine iter
    return (line, rx, ry, rw, rh)

  case line of
    Nothing -> pure ()
    Just l -> do
      Cairo.translate rx 0
      Pango.showLayoutLine l
      Cairo.translate (negate rx) (offset)
  b <- Cairo.liftIO $ Pango.layoutIterNextLine iter
  if b then showLayoutLines iter offset else pure ()

-- utility functions

proceedToLastLine :: Pango.LayoutIter -> IO ()
proceedToLastLine it = do
  b <- Pango.layoutIterAtLastLine it
  if b
    then return ()
    else do
      Pango.layoutIterNextLine it
      proceedToLastLine it

pcdOptionsMap = Map.fromList [
    ("ContentRotation", \r v -> r { pcdOptContentRotation = Just v }) 

  , ("MaxDPI", \r v -> r { pcdOptMaxDPI = Just $ read v }) 
  , ("radius", \r v -> r { pcdOptRadius = Just $ read v }) 

  , ("FontSize", \r v -> r { pcdOptFontSize = Just v })
  , ("FontSizeCJK", \r v -> r { pcdOptFontSizeCJK = Just v })
  , ("Typeface", \r v -> r { pcdOptTypeface = Just v })
  , ("TypefaceCJK", \r v -> r { pcdOptTypefaceCJK = Just v })
  , ("TextAlign", \r v -> r { pcdOptTextAlign = Just $ case v of
                                                         "left" -> Pango.AlignLeft
                                                         "center" -> Pango.AlignCenter
                                                         "right" -> Pango.AlignRight
                                                         _ -> Pango.AlignLeft })
  , ("TextVerticalAlign", \r v -> r { pcdOptTextVerticalAlign = Just v })

  , ("BaselineOffset", \r v -> r { pcdOptBaselineOffset = Just $ round $ (read v :: Double) * Pango.pangoScale })
  , ("BaselineOffsetCJK", \r v -> r { pcdOptBaselineOffsetCJK = Just $ round $ (read v :: Double) * Pango.pangoScale })
  , ("Kerning", \r v -> r { pcdOptKerning = Just $ round $ (read v :: Double) * 1024 })
  , ("KerningCJK", \r v -> r { pcdOptKerningCJK = Just $ round $ (read v :: Double) * 1024 })
  , ("LineSpacing", \r v -> r { pcdOptLineSpacing = Just $ read v })
  , ("LineHeight", \r v -> r { pcdOptLineHeight = Just $ read v })
  , ("Ligature", \r v -> r { pcdOptLigature = Just $ v == "1" })

  , ("SymbolSubstitution", \r v -> r { pcdOptSymbolSubstitution = Just v })
  , ("TypefaceSubstitution", \r v -> r { pcdOptTypefaceSubstitution = Just v })
  , ("FontSizeSubstitution", \r v -> r { pcdOptFontSizeSubstitution = Just v })
  ]

cmykToRGBA :: PCDColor -> StateT (Map.Map PCDColor PCDColor) IO PCDColor
cmykToRGBA (RGBA r g b a) = return $ RGBA r g b a
cmykToRGBA cmyk@(CMYK c m y k a) = do
  map <- get
  case Map.lookup cmyk map of
    Nothing -> do
      rgbastr <- liftIO $ readProcess "transicc" [ "-i", "res/CMYK.icc", "-o", "res/sRGB.icc", "-n" ]
                                                 (concat [show (c*100), "%\n", show(m*100), "%\n", show (y*100), "%\n", show (k*100), "%"])

      case words rgbastr of
        [r, g, b] -> do
          let rgba = RGBA (read r / 255) (read g / 255) (read b / 255) a
          put (Map.insert cmyk rgba map)
          return rgba
        _ -> fail "cmykToRGBA conversion error"
    Just rgba -> return rgba

preserveMatrix :: Cairo.Render () -> Cairo.Render ()
preserveMatrix r = do
  m <- Cairo.getMatrix
  r
  Cairo.setMatrix m

unmarkImageOpts :: PCDOptions -> PCDOptions
unmarkImageOpts opts =
  opts { pcdOptMaxDPI = Nothing
       , pcdOptContentRotation = Nothing
       , pcdOptRadius = Nothing
       }

unmarkTextOpts :: PCDOptions -> PCDOptions
unmarkTextOpts opts =
  opts { pcdOptFontSize = Nothing
       , pcdOptFontSizeCJK = Nothing
       , pcdOptTypeface = Nothing
       , pcdOptTypefaceCJK = Nothing
       , pcdOptTextAlign = Nothing
       , pcdOptTextVerticalAlign = Nothing
       , pcdOptBaselineOffset = Nothing
       , pcdOptBaselineOffsetCJK = Nothing
       , pcdOptKerning = Nothing
       , pcdOptKerningCJK = Nothing
       , pcdOptLineSpacing = Nothing
       , pcdOptColor = Nothing
       , pcdOptLineHeight = Nothing
       , pcdOptLigature = Nothing
       , pcdOptSymbolSubstitution = Nothing
       , pcdOptTypefaceSubstitution = Nothing
       , pcdOptFontSizeSubstitution = Nothing
       , pcdOptContentRotation = Nothing
       }

isCJK :: Char -> Bool
isCJK c = o >= 0x2e80 || elem o [0x2013, 0x2014, 0x2025, 0x2026, 0x22ee, 0x22ef, 0x2500, 0x2502, 0x2048, 0x2049]
  where o = ord c

data MarkupKind = Subst | CJK | Latin deriving (Eq, Show)

markupText :: (String, String, (String, String)) -> String -> String
markupText _ [] = []
markupText attrs@(latinAttrs, cjkAttrs, (substSymbols, substAttrs)) (x:xs) = (spanAttrs (x:ys)) ++ (markupText attrs zs)
  where (ys,zs) = span (\c -> (kind x) == (kind c)) xs
        spanAttrs s = "<span " ++ (case kind x of { Subst -> substAttrs; CJK -> cjkAttrs; _ -> latinAttrs }) ++ ">" ++ s ++ "</span>"
        kind c = if elem c substSymbols then Subst else if isCJK c then CJK else Latin

buildMarkupAttrs :: PCDOptions -> (String, String, (String, String))
buildMarkupAttrs = (,,) <$> buildLatin <*> buildCJK <*> buildSubst
  where buildLatin opts = let typeface = fromMaybe "Helvetica" (pcdOptTypeface opts)
                              fontSize = fromMaybe "16" (pcdOptFontSize opts)
                              kerning = fromMaybe 0 (pcdOptKerning opts)
                              baselineOffset = fromMaybe 0 (pcdOptBaselineOffset opts)
                          in (   "font_desc=\"" ++ (intercalate " " (filter (not . null) [typeface, fontSize])) ++ "\""
                             ++ " letter_spacing=\"" ++ (show kerning)  ++ "\""
                             ++ " rise=\"" ++ (show $ baselineOffset) ++ "\""
                             )
        buildCJK opts = let typeface = fromMaybe "Hiragino Sans GB W3" (pcdOptTypefaceCJK opts)
                            fontSize = fromMaybe "16" (pcdOptFontSizeCJK opts)
                            kerning = fromMaybe 0 (pcdOptKerningCJK opts)
                            baselineOffset = fromMaybe 0 (pcdOptBaselineOffsetCJK opts)
                          in (   "font_desc=\"" ++ (intercalate " " (filter (not . null) [typeface, fontSize])) ++ "\" "
                             ++ " letter_spacing=\"" ++ (show kerning) ++ "\""
                             ++ " rise=\"" ++ (show $ baselineOffset) ++ "\""
                             )
        buildSubst opts = let symbols = fromMaybe "" (pcdOptSymbolSubstitution opts)
                              typeface = fromMaybe "Heiti TC Light" (pcdOptTypefaceSubstitution opts)
                              fontSize = fromMaybe "16" (pcdOptFontSizeSubstitution opts)
                          in (symbols, "font_desc=\"" ++ (intercalate " " (filter (not . null) [typeface, fontSize])) ++ "\"")
