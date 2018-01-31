module PageComposerSpec where

import Test.Hspec

import PCD
import PageComposer
import System.Exit (ExitCode(ExitFailure))

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "simplecolor" $ do
    it "/tmp/test_simplecolor.pdf" $ do
      let cs = [ SimpleColor (CMYK 0.0 0.7412 0.851 0.0 1.0) 10 150 80 50 ]
          pcd = PCD 200 200 cs (PCD2PDF "/tmp/test_simplecolor.pdf")

      runPCD pcd Nothing
  
  describe "simpleimage" $ do
    it "/tmp/test_simpleimage.pdf" $ do
      let cs = [ SimpleImage "/tmp/test_photo.jpg" 50 0 500 750 ] 
          pcd = PCD 800 800 cs (PCD2PDF "/tmp/test_simpleimage.pdf")

      runPCD pcd Nothing

    it "/tmp/test_simpleimage_clockwise.pdf" $ do
      let cs = [ Option "ContentRotation" "clockwise"
               , SimpleImage "/tmp/test_photo.jpg" 50 0 500 750 ] 
          pcd = PCD 800 800 cs (PCD2PDF "/tmp/test_simpleimage_clockwise.pdf")

      runPCD pcd Nothing

    it "/tmp/test_simpleimage_counter-clockwise.pdf" $ do
      let cs = [ Option "ContentRotation" "counter-clockwise"
               , SimpleImage "/tmp/test_photo.jpg" 50 0 500 750 ] 
          pcd = PCD 800 800 cs (PCD2PDF "/tmp/test_simpleimage_counter-clockwise.pdf")

      runPCD pcd Nothing

    it "/tmp/test_simpleimage_half.pdf" $ do
      let cs = [ Option "ContentRotation" "half"
               , SimpleImage "/tmp/test_photo.jpg" 50 0 500 750 ] 
          pcd = PCD 800 800 cs (PCD2PDF "/tmp/test_simpleimage_half.pdf")

      runPCD pcd Nothing

  describe "image" $ do
    it "/tmp/test_image.pdf" $ do
      let cs = [ Image "/tmp/test_photo.jpg" "675" "274" "2367" "1740" 50 0 500 750 ] 
          pcd = PCD 800 800 cs (PCD2PDF "/tmp/test_image.pdf")

      runPCD pcd Nothing

    it "/tmp/test_image_maxDPI.pdf" $ do
      let cs = [ Option "MaxDPI" "50"
               , Image "/tmp/test_photo.jpg" "675" "274" "2367" "1740" 50 0 500 750 ] 
          pcd = PCD 800 800 cs (PCD2PDF "/tmp/test_image_maxDPI.pdf")

      runPCD pcd Nothing


    it "/tmp/test_image_clockwise.pdf" $ do
      let cs = [ Option "ContentRotation" "clockwise"
               , Image "/tmp/test_photo.jpg" "675" "274" "2367" "1740" 50 0 500 750 ] 
          pcd = PCD 800 800 cs (PCD2PDF "/tmp/test_image_clockwise.pdf")

      runPCD pcd Nothing

    it "/tmp/test_image_counter-clockwise.pdf" $ do
      let cs = [ Option "ContentRotation" "counter-clockwise"
               , Image "/tmp/test_photo.jpg" "675" "274" "2367" "1740" 50 0 500 750 ] 
          pcd = PCD 800 800 cs (PCD2PDF "/tmp/test_image_counter-clockwise.pdf")

      runPCD pcd Nothing

    it "/tmp/test_image_half.pdf" $ do
      let cs = [ Option "ContentRotation" "half"
               , Image "/tmp/test_photo.jpg" "675" "274" "2367" "1740" 50 0 500 750 ] 
          pcd = PCD 800 800 cs (PCD2PDF "/tmp/test_image_half.pdf")

      runPCD pcd Nothing

    it "/tmp/test_image_arc.pdf" $ do
      let cs = [ Option "radius" "30"
               , Image "/tmp/test_photo.jpg" "675" "274" "2367" "1740" 50 0 500 750 ] 
          pcd = PCD 800 800 cs (PCD2PDF "/tmp/test_image_arc.pdf")

      runPCD pcd Nothing

    --it "/tmp/test_image_pdf.pdf" $ do
    --  let cs = [ Image "/tmp/test_photo.pdf" "675" "274" "2367" "1740" 50 0 500 750 ] 
    --      pcd = PCD 800 800 cs (PCD2PDF "/tmp/test_image_pdf.pdf")

    --  runPCD pcd Nothing

  describe "image_compress" $ do
    it "/tmp/test_image_compress_clockwise.pdf" $ do
      let cs = [ Option "ContentRotation" "clockwise"
               , ImageCompress "/tmp/test_photo.jpg" 0.99 "675" "274" "2367" "1740" 50 0 500 750 ] 
          pcd = PCD 800 800 cs (PCD2PDF "/tmp/test_image_compress_clockwise.pdf")

      runPCD pcd Nothing

  describe "simpletext" $ do
    it "/tmp/test_simpletext.pdf" $ do
      let cs = [ SimpleText 0 20 "Hello! 中文什麼"
               , SimpleColor (RGBA 1.0 0.0 1.0 1.0) 0 20 20 20 ]
          pcd = PCD 100 100 cs (PCD2PDF "/tmp/test_simpletext.pdf")

      runPCD pcd Nothing

  describe "text" $ do
    it "/tmp/test_text.pdf" $ do
      let cs = [ Text False False 0 80 80 20 "Hi! 什麼" ]
          pcd = PCD 100 100 cs (PCD2PDF "/tmp/test_text.pdf")

      runPCD pcd Nothing
    
    it "/tmp/test_text_line_height.pdf" $ do
      let cs = [ Option "LineHeight" "24"
               --, Option "LineSpacing" "2"
               , Text False False 0 0 100 100 "Hi!\nHello!\nYou!\nHey!\n" ]
          pcd = PCD 100 100 cs (PCD2PDF "/tmp/test_text_line_height.pdf")

      runPCD pcd Nothing
 

    it "/tmp/test_text_attrs.pdf" $ do
      let cs = [ Option "Typeface" "Arial"
               , Option "TypefaceCJK" "PingFang TC"
               , Option "FontSize" "9"
               , Option "FontSizeCJK" "15.0"
               , Option "LineSpacing" "10"
               , Option "TextAlign" "right"
               , Option "Color" "cheese"
               , Text False False 0 30 100 60 "Hello!\n你好嗎"
               , Option "Typeface" "Arial"
               , Option "TypefaceCJK" "PingFang TC"
               , Option "FontSize" "9"
               , Option "FontSizeCJK" "15.0"
               , Option "LineSpacing" "10"
               , Option "Kerning" "5"
               , Option "KerningCJK" "5.0"
               , Option "TextAlign" "left"
               , Option "Color" "cyan"
               , Text False False 0 30 100 60 "Hello!\n你好嗎"
               , Option "BaselineOffsetCJK" "5"
               , Option "Color" "purple"
               , Option "TextAlign" "right"
               , Text False False 0 0 100 24 "幹\n什麼"
               , Option "BaselineOffsetCJK" "0"
               , Option "Color" "red"
               , Option "TextAlign" "left"
               , Text False False 0 0 100 24 "幹\n什麼"
               ]
          pcd = PCD 100 100 cs (PCD2PDF "/tmp/test_text_attrs.pdf")

      runPCD pcd Nothing

    it "/tmp/test_text_vertical-align.pdf" $ do
      let cs = [ Option "TextVerticalAlign" "top"
               , Text False False 0 0 250 100 "I use top alignment"
               , Option "TextVerticalAlign" "middle"
               , Text False False 0 0 250 100 "I use middle alignment"
               , Option "TextVerticalAlign" "bottom"
               , Text False False 0 0 250 100 "I use bottom alignment"
               , Option "TextVerticalAlign" "bottom-baseline:12"
               , Text False False 0 0 250 100 "I use bottom-baseline:12 alignment"
               ]
          pcd = PCD 250 100 cs (PCD2PDF "/tmp/test_text_vertical-align.pdf")

      runPCD pcd Nothing

--    it "/tmp/test_text_vertical_align.pdf" $ do
--      let cs = [ SimpleColor (RGBA 238 255 153) 0 0 200 10
--               , SimpleColor (RGBA 136 153 255) 0 10 200 190
--               , Option "FontSize" "22"
--               , Option "FontSizeCJK" "22"
--               , 
--
--               , ]

    it "/tmp/test_text_subst.pdf" $ do
      let cs = [ Option "SymbolSubstitution" "，。、；？！"
               , Option "TypefaceSubstitution" "STHeitiTC-Light"
               , Option "FontSizeSubstitution" "36"
               , Option "FontSizeCJK" "16"
               , Text False False 0 0 250 60 "Hello! 你好！吃飽沒？"]
          pcd = PCD 250 60 cs (PCD2PDF "/tmp/test_text_subst.pdf")
      runPCD pcd Nothing
    
  describe "text_checksize" $ do
    it "/tmp/test_text_checksize.pdf" $ do
      let cs = [ Text True False 0 80 60 12 "Hello world!" ]
          pcd = PCD 100 100 cs (PCD2PDF "/tmp/test_text_checksize.pdf")

      runPCD pcd Nothing `shouldThrow` (== ExitFailure 1)
  
  describe "text_vtext" $ do
    it "/tmp/test_text_clockwise.pdf" $ do
      let cs = [ SimpleColor (RGBA 0.5 0.5 0.5 1.0) 0 0 24 100
               , SimpleColor (RGBA 0.7 0.7 0.7 1.0) 0 0 100 24
               --, Option "TextVerticalAlign" "bottom"
               --, Option "TextAlign" "right"
               --, Text False False 0 0 100 24 "Hi! 你好！" 
               , Option "TextVerticalAlign" "bottom"
               , Option "FontSize" "16"
               , Option "FontSizeCJK" "16"
               , Text False False 0 0 100 24 "H! 你！" 
               , Option "FontSize" "17"
               , Option "FontSizeCJK" "17"
               , Option "Color" "purple"
               , Option "TextVerticalAlign" "bottom"
               , Text False False 0 0 100 24 "H! 你！" 

               , Option "ContentRotation" "clockwise"
               , Option "TextVerticalAlign" "top"
               , Text False False 0 0 24 100 "Hi! 你好！"
               , Option "ContentRotation" "clockwise"
               , Option "TextVerticalAlign" "bottom"
               , Text False False 0 0 24 100 "Hi! 你好！" ]
          pcd = PCD 100 100 cs (PCD2PDF "/tmp/test_text_clockwise.pdf")
      
      runPCD pcd Nothing

    it "/tmp/test_text_vtext.pdf" $ do
      let cs = [ Text False True 0 0 24 100 "Hi! 你好！" ]
          pcd = PCD 100 100 cs (PCD2PDF "/tmp/test_text_vtext.pdf")

      runPCD pcd Nothing

    it "/tmp/test_text_vtext.pdf" $ do
      let cs = [ Text False True 0 0 24 100 "Hi! 你好！" ]
          pcd = PCD 100 100 cs (PCD2PDF "/tmp/test_text_vtext.pdf")

      runPCD pcd Nothing

    it "/tmp/test_vtext_vertical-align.pdf" $ do
      let cs = [ Option "TextVerticalAlign" "top"
               , Option "TextAlign" "right"
               , Text False True 0 0 100 350 "I use right alignment."
               , Option "TextVerticalAlign" "top"
               , Option "TextAlign" "left"
               , Text False True 0 0 100 350 "I use left alignment."

               , Option "TextVerticalAlign" "middle"
               , Option "TextAlign" "right"
               , Text False True 0 0 100 350 "I use right alignment."
               , Option "TextVerticalAlign" "middle"
               , Option "TextAlign" "left"
               , Text False True 0 0 100 350 "I use left alignment."


               , Option "TextVerticalAlign" "bottom"
               , Option "TextAlign" "right"
               , Text False True 0 0 100 350 "I use right alignment."
               , Option "TextVerticalAlign" "bottom"
               , Option "TextAlign" "left"
               , Text False True 0 0 100 350 "I use left alignment."
               ]
          pcd = PCD 100 350 cs (PCD2PDF "/tmp/test_vtext_vertical-align.pdf")

      runPCD pcd Nothing

    it "/tmp/test_vtext_line_height.pdf" $ do
      let cs = [ Option "LineHeight" "24"
               , Text False True 0 0 100 100 "Hi!\nHello!\nYou!\nHey!\n" ]
          pcd = PCD 100 100 cs (PCD2PDF "/tmp/test_vtext_line_height.pdf")

      runPCD pcd Nothing

  describe "endjpg" $ do
    it "/tmp/test_endjpg_jdc.jpg" $ do
      let cs = [ Text False True 0 0 24 100 "Hi! 你你你你好！" ]
          pcd = PCD 100 100 cs (PCD2JPG "/tmp/test_endjpg_jdc.jpg" (JDC 300 0.99))

      runPCD pcd Nothing

    it "/tmp/test_endjpg_jwhc.jpg" $ do
      let cs = [ Text False True 0 0 24 100 "Hi! 你你你你好！" ]
          pcd = PCD 100 100 cs (PCD2JPG "/tmp/test_endjpg_jwhc.jpg" (JWHC 420 420 0.99))

      runPCD pcd Nothing

  describe "endpng" $ do
    it "/tmp/test_endpng_pds.jpg" $ do
      let cs = [ Text False True 0 0 24 100 "Hi! 你你你你好！" ]
          pcd = PCD 100 100 cs (PCD2PNG "/tmp/test_endpng_pds.jpg" (PDS 300 1.0))

      runPCD pcd Nothing

    it "/tmp/test_endpng_pwh.jpg" $ do
      let cs = [ Text False True 0 0 24 100 "Hi! 你你你你好！" ]
          pcd = PCD 100 100 cs (PCD2PNG "/tmp/test_endpng_pwh.jpg" (PWH 420 420))

      runPCD pcd Nothing

--
  --describe "demo" $ do
  --  it "/tmp/demo.pdf" $ do
  --    let cs = [ Option "TypefaceCJK" "Yuanti TC"
  --             , Option "FontSize" "9"
  --             , Option "FontSizeCJK" "15"
  --             , SimpleImage "/tmp/test_photo.jpg" 0 50 50 50
  --             , Option "ContentRotation" "clockwise"
  --             , SimpleImage "/tmp/test_photo.jpg" 50 50 50 50
  --             , Text 0 0 100 30 "Hello! 你好"
  --             , SimpleColor (RGBA 1.0 0.0 1.0 1.0) 35 35 30 30]
  --             --,
  --             
  --        pcd = PCD 100 100 cs (PCD2PDF "/tmp/demo.pdf")
  --    runPCD pcd Nothing

 
