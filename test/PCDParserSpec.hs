{-# LANGUAGE OverloadedStrings #-}
module PCDParserSpec where

import Data.Either (isLeft)
import Test.Hspec
import qualified Data.Text as T

import PCD
import PCDParser
import Text.Megaparsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "utility functions" $ do
    it "parses double" $ do
      (parse rawDouble "" "-3.14159e-7") `shouldBe` Right "-3.14159e-7"

  describe "basic cases" $ do
    it "parses endpdf" $ do
      let path = "/Mars/my.pdf"
          fpath = "file://" ++ path
          pcd = T.unlines $ T.pack <$> [ "beginpdf 123 456", "endpdf " ++  fpath ]
      parsePCD pcd `shouldBe` Right (PCD 123.0 456.0 [] (PCD2PDF path))

    it "parses endjpg" $ do
      let path = "/Mars/my.jpg"
          fpath = "file://" ++ path
          pcd = T.unlines $ T.pack <$> [ "beginpdf 123 456"
                                       , "endjpg 192px 168.0px 1.0 " ++ fpath ]
      parsePCD pcd `shouldBe` Right (PCD 123.0 456.0 [] (PCD2JPG path (JWHC 192.0 168.0 1.0)))

    it "parses endpng" $ do
      let path = "/Mars/my.png"
          fpath = "file://" ++ path
          pcd = T.unlines $ T.pack <$> [ "beginpdf 123 456"
                                       , "endpng 192px 168.0px " ++ fpath ]
      parsePCD pcd `shouldBe` Right (PCD 123.0 456.0 [] (PCD2PNG path (PWH 192.0 168.0)))

  describe "essential cases" $ do
    it "parses without pt" $ do
      let cs = [ "set FontSize 9pt"
               , "simpleimage http://hypo.cc/test.jpg 1pt 2pt 3pt 4pt" 
               , "image http://hypo.cc/test.jpg 1pt 2pt 3pt 4pt 5pt 6pt 7pt 8pt" 
               , "simpletext 1pt 2pt \"Helll ooooooo\""
               ]
          Right (PCD _ _ parsedCs _) = parsePCD (pcdT cs)
      parsedCs `shouldBe` [ Option "FontSize" "9.0"
                          , SimpleImage "/test.jpg" 1.0 2.0 3.0 4.0
                          , Image "/test.jpg" "1.0" "2.0" "3.0" "4.0" 5.0 6.0 7.0 8.0
                          , SimpleText 1.0 2.0 "Helll ooooooo"
                          ]
--
    it "parses with options" $ do
      let cs = [ "simpleimage http://hypo.cc/test.jpg 1 2 3 4"
               , "set ContentRotation clockwise", "set radius 3.0"
               , "simpleimage http://hypo.cc/test.jpg 1 2 3 4"  ]
          Right (PCD _ _ parsedCs _) = parsePCD (pcdT cs)
      parsedCs `shouldBe` [ SimpleImage "/test.jpg" 1.0 2.0 3.0 4.0
                          , Option "ContentRotation" "clockwise"
                          , Option "radius" "3.0"
                          , SimpleImage "/test.jpg" 1.0 2.0 3.0 4.0 ]

    it "parses RGB" $ do
      let cs = [ "simplecolor 0xFFCCBB 0 1 2 3" ]
          Right (PCD _ _ parsedCs _) = parsePCD (pcdT cs)
      parsedCs `shouldBe` [ SimpleColor (RGBA (255/255) (204/255) (187/255) 1.0) 0 1 2 3 ]

    it "rejects malformed RGB" $ do
      let cs = [ "simplecolor 0xFFTTBB 0 1 2 3" ]
      isLeft (parsePCD (pcdT cs)) `shouldBe` True

    it "parses CMYK" $ do
      let cs = [ "simplecolor CMYK:0.00, 1.00, 1.00, 0.00 0 1 2 3" ]
          Right (PCD _ _ parsedCs _) = parsePCD (pcdT cs)
      parsedCs `shouldBe` [ SimpleColor (CMYK 0.0 1.00 1.00 0.0 1.0) 0 1 2 3 ]

    it "parses CMYK (with alpha channel)" $ do
      let cs = [ "simplecolor CMYK:0.00, 1.00, 0.00, 0.00, 0.80 0 1 2 3" ]
          Right (PCD _ _ parsedCs _) = parsePCD (pcdT cs)
      parsedCs `shouldBe` [ SimpleColor (CMYK 0.0 1.00 0.00 0.0 0.8) 0 1 2 3 ]
 
    it "parses common colors" $ do
      let cs = [ "simplecolor grass 0 1 2 3" ]
          Right (PCD _ _ parsedCs _) = parsePCD (pcdT cs)
      parsedCs `shouldBe` [ SimpleColor (CMYK (0.35) (0.0) (1.0) (0.0) (1.0)) 0 1 2 3 ]

  describe "commands" $ do
    it "parses SimpleColor" $ do
      let cs = [ "simplecolor 0xFFFFFF 0pt 1pt 2pt 3" ]
          Right (PCD _ _ parsedCs _) = parsePCD (pcdT cs)
      parsedCs `shouldBe` [ SimpleColor (RGBA 1.0 1.0 1.0 1.0) 0 1 2 3 ]

    it "parses SimpleImage" $ do
      let cs = [ "simpleimage file:///tmp/test_photo.jpg 0.5pt 0.4 0.3 0.5pt" ]
          Right (PCD _ _ parsedCs _) = parsePCD (pcdT cs)
      parsedCs `shouldBe` [ SimpleImage "/tmp/test_photo.jpg" 0.5 0.4 0.3 0.5 ]

    it "parses Image" $ do
      let cs = [ "image file:///tmp/test_photo.jpg x0.01 X0.05 *0.5 *1 0.0pt 1.0 2 3pt" ]
          Right (PCD _ _ parsedCs _) = parsePCD (pcdT cs)
      parsedCs `shouldBe` [ Image "/tmp/test_photo.jpg" "x0.01" "X0.05" "*0.5" "*1" 0.0 1.0 2.0 3.0 ]

    it "parses SimpleImageCompress" $ do
      let cs = [ "simpleimage_compress file:///tmp/test_photo.jpg 0.99 0.5pt 0.4 0.3 0.5pt" ]
          Right (PCD _ _ parsedCs _) = parsePCD (pcdT cs)
      parsedCs `shouldBe` [ SimpleImageCompress "/tmp/test_photo.jpg" 0.99 0.5 0.4 0.3 0.5 ]
      
    it "parses ImageCompress" $ do
      let cs = [ "image_compress file:///tmp/test_photo.jpg 0.98 x0.01 X0.05 *0.5 *1 0.0pt 1.0 2 3pt" ]
          Right (PCD _ _ parsedCs _) = parsePCD (pcdT cs)
      parsedCs `shouldBe` [ ImageCompress "/tmp/test_photo.jpg" 0.98 "x0.01" "X0.05" "*0.5" "*1" 0.0 1.0 2.0 3.0 ]

    it "parses SimpleText" $ do
      let cs = [ "simpletext 1.0 2pt \"Hello World\"" ]
          Right (PCD _ _ parsedCs _) = parsePCD (pcdT cs)
      parsedCs `shouldBe` [ SimpleText 1.0 2.0 "Hello World" ]

    it "parses Text" $ do
      let cs = [ "text 1.0 2pt 3 4pt \"Hello World\"" ]
          Right (PCD _ _ parsedCs _) = parsePCD (pcdT cs)
      parsedCs `shouldBe` [ Text False False 1.0 2.0 3.0 4.0 "Hello World" ]

    it "parses TextChecksize" $ do
      let cs = [ "text_checksize 1.0 2pt 3 4pt \"Hello World\"" ]
          Right (PCD _ _ parsedCs _) = parsePCD (pcdT cs)
      parsedCs `shouldBe` [ Text True False 1.0 2.0 3.0 4.0 "Hello World" ]

    it "parses VText" $ do
      let cs = [ "vtext 1.0 2pt 3 4pt \"Hello World\"" ]
          Right (PCD _ _ parsedCs _) = parsePCD (pcdT cs)
      parsedCs `shouldBe` [ Text False True 1.0 2.0 3.0 4.0 "Hello World" ]

  describe "bugs" $ do
    it "parses image_compress" $ do
      let cs = [ "image_compress file://localhost/141880_0E4221E4_o.jpg 0.99 306.33734196444567 0.0 918.9999999999999 919.0 76.53629133858269 209.76729133858268 255.118 255.118" ]
          Right (PCD _ _ parsedCs _) = parsePCD (pcdT cs)
      parsedCs `shouldBe` [ ImageCompress "/141880_0E4221E4_o.jpg" 0.99 "306.33734196444567" "0.0" "918.9999999999999" "919.0" (read "76.53629133858269") (read "209.76729133858268") (read "255.118") (read "255.118") ]

    it "parses setColor" $ do
      let cs = [ "set Color 0xFFFFFF" ]
          Right (PCD _ _ parsedCs _) = parsePCD (pcdT cs)

      parsedCs `shouldBe` [ ColorOption (Just (RGBA 1.0 1.0 1.0 1.0)) ]

    it "parses opts based on newline" $ do
      let cs = ["set Typeface Univers Light"]
          Right (PCD _ _ parsedCs _) = parsePCD (pcdT cs)
      parsedCs `shouldBe` [ Option "Typeface" "Univers Light" ]


  where pcdT cs = T.unlines $ [ "beginpdf 123 456" ] ++ cs ++ [ "endpdf file://abc.pdf" ]

 
