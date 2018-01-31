module PCD where

import qualified Graphics.Rendering.Pango as Pango

data PCD = PCD {
    pcdWidth  :: Double
  , pcdHeight :: Double
  , pcdContents :: [PCDContent]
  , pcdOutput :: PCDOutput
} deriving (Eq, Show)

data PCDOutput = PCD2PDF String
               | PCD2JPG String JPGOption
               | PCD2PNG String PNGOption
  deriving (Eq, Show)

data JPGOption = JDC Integer Double
               | JWHC Double Double Double
               deriving (Eq, Show)

data PNGOption = PDS Integer Double
               | PWH Double Double
               deriving (Eq, Show)

data PCDContent =
    Option String String
  | ColorOption (Maybe PCDColor)
  | SimpleImage         String Double Double Double Double
  | SimpleImageCompress String Double Double Double Double Double
  | Image         String String String String String Double Double Double Double
  | ImageCompress String Double String String String String Double Double Double Double
  | SimpleColor PCDColor Double Double Double Double
  | SimpleText Double Double String
  | Text Bool Bool Double Double Double Double String
  deriving (Eq, Show)

data PCDColor = RGBA Double Double Double Double
              | CMYK Double Double Double Double Double
              deriving (Eq, Show, Ord)

data PCDOptions = PCDOptions {
  -- shared
    pcdOptContentRotation :: Maybe String

  -- images
  , pcdOptMaxDPI :: Maybe Double
  , pcdOptRadius :: Maybe Double

  -- text
  , pcdOptFontSize :: Maybe String
  , pcdOptFontSizeCJK :: Maybe String
  , pcdOptTypeface :: Maybe String
  , pcdOptTypefaceCJK :: Maybe String
  , pcdOptTextAlign :: Maybe Pango.LayoutAlignment
  , pcdOptTextVerticalAlign :: Maybe String

  , pcdOptBaselineOffset :: Maybe Integer
  , pcdOptBaselineOffsetCJK :: Maybe Integer
  , pcdOptKerning :: Maybe Integer
  , pcdOptKerningCJK :: Maybe Integer
  , pcdOptLineSpacing :: Maybe Double
  , pcdOptColor :: Maybe PCDColor
  , pcdOptLineHeight :: Maybe Double
  , pcdOptLigature :: Maybe Bool
  
  , pcdOptSymbolSubstitution :: Maybe String
  , pcdOptTypefaceSubstitution :: Maybe String
  , pcdOptFontSizeSubstitution :: Maybe String
}

initPCDOptions = PCDOptions {
  -- shared
    pcdOptContentRotation = Nothing

  -- images
  , pcdOptMaxDPI = Nothing
  , pcdOptRadius = Nothing

  -- text
  , pcdOptFontSize = Nothing
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
}

