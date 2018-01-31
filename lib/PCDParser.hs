module PCDParser where

import Data.Char
import qualified Data.Map as Map
import Data.List (isPrefixOf, intercalate)
import Control.Monad (void)
import Numeric (readHex)

import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L

import Network.URI (URI(uriPath), parseURIReference)

import PCD

parsePCD = parse pcdParser ""

extractParseError :: ParseError -> String
extractParseError = intercalate "\n" . fmap messageString . errorMessages

pcdParser :: Parser PCD
pcdParser = do
  (w, h) <- parsePCDBegin
  cs <- parseContents
  o <- parsePCDEnd
  return $ PCD w h cs o

parsePCDBegin :: Parser (Double, Double)
parsePCDBegin = do
  _ <- symbol "beginpdf"
  (,) <$> doublePt <*> doublePt

parsePCDEnd :: Parser PCDOutput
parsePCDEnd = try parseEndpdf <|> try parseEndpng <|> parseEndjpg

parseEndpdf :: Parser PCDOutput
parseEndpdf = do
  _ <- symbol "endpdf"
  f <- pcdfilepath
  return $ PCD2PDF f

parseEndjpg :: Parser PCDOutput
parseEndjpg = do
  _ <- symbol "endjpg"
  o <- choice [try dc, whc]
  f <- pcdfilepath
  return $ PCD2JPG f o
  where dc = JDC <$> (round <$> double) <*> double
        whc = JWHC <$> (double <* symbol "px")
                   <*> (double <* symbol "px")
                   <*> double

parseEndpng :: Parser PCDOutput
parseEndpng = do
  _ <- symbol "endpng"
  o <- choice [try ds, whc]
  f <- pcdfilepath
  return $ PCD2PNG f o
  where ds = PDS <$> (round <$> double) <*> double
        whc = PWH <$> (double <* symbol "px")
                  <*> (double <* symbol "px")

--

parseContents :: Parser [PCDContent]
parseContents = many $ choice [ try parseColorOption
                              , try parseOption
                              , try parseSimpleImageCompress
                              , try parseSimpleImage
                              , try parseImageCompress
                              , try parseImage
                              , try parseSimpleColor
                              , try parseSimpleText
                              , parseText ]

parseOption :: Parser PCDContent
parseOption = do
  _ <- symbol "set"
  Option <$> pcdstring <*> optstringPt

parseColorOption :: Parser PCDContent
parseColorOption = do
  _ <- symbol "set"
  _ <- symbol "Color"
  ColorOption <$> (option Nothing (Just <$> pcdcolor))

parseSimpleImage :: Parser PCDContent
parseSimpleImage = do
  _ <- symbol "simpleimage"
  SimpleImage <$> pcdurl
              <*> doublePt <*> doublePt <*> doublePt <*> doublePt

parseSimpleImageCompress :: Parser PCDContent
parseSimpleImageCompress = do
  _ <- symbol "simpleimage_compress"
  SimpleImageCompress <$> pcdurl <*> double
              <*> doublePt <*> doublePt <*> doublePt <*> doublePt

parseImage :: Parser PCDContent
parseImage = do
  _ <- symbol "image"
  Image <$> pcdurl
        <*> pcdstringPt <*> pcdstringPt <*> pcdstringPt <*> pcdstringPt
        <*> doublePt <*> doublePt <*> doublePt <*> doublePt

parseImageCompress :: Parser PCDContent
parseImageCompress = do
  _ <- symbol "image_compress"
  ImageCompress <$> pcdurl <*> double
        <*> pcdstringPt <*> pcdstringPt <*> pcdstringPt <*> pcdstringPt
        <*> doublePt <*> doublePt <*> doublePt <*> doublePt

parseSimpleColor :: Parser PCDContent
parseSimpleColor = do
  _ <- symbol "simplecolor"
  SimpleColor <$> pcdcolor
              <*> doublePt <*> doublePt <*> doublePt <*> doublePt

parseSimpleText :: Parser PCDContent
parseSimpleText = do
  _ <- symbol "simpletext"
  SimpleText <$> doublePt <*> doublePt
             <*> pcdstring

parseText :: Parser PCDContent
parseText = choice [try text, try checkSize, vtext]
  where text = do
          _ <- symbol "text"
          Text False False <$> doublePt <*> doublePt <*> doublePt <*> doublePt <*> pcdstring
        checkSize = do
          _ <- symbol "text_checksize"
          Text True False <$> doublePt <*> doublePt <*> doublePt <*> doublePt <*> pcdstring
        vtext = do
          _ <- symbol "vtext"
          Text False True <$> doublePt <*> doublePt <*> doublePt <*> doublePt <*> pcdstring
--

lexeme :: Parser a -> Parser a
lexeme p = p <* space

symbol :: String -> Parser String
symbol s = lexeme $ string s

integer :: Parser Integer
integer = read <$> lexeme (some digitChar)

pcdfilepath :: Parser String
pcdfilepath = do
  rawstr <- pcdstring
  case uriPath <$> parseURIReference rawstr of
    Nothing -> return "/tmp/fallback.pdf"
    Just path -> return path 

pcdurl :: Parser String
pcdurl = do
  rawstr <- pcdstring
  case uriPath <$> parseURIReference rawstr of
    Nothing -> fail "unrecognized url"
    Just path -> return path

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

pcdstring :: Parser String
pcdstring = lexeme $ stringLiteral <|> many (satisfy (not . isSpace))

optstring :: Parser String
optstring = lexeme $ stringLiteral <|> many (satisfy (/= '\n'))

rstripPt :: Parser a -> Parser a
rstripPt pa = pa <* (try $ optional $ string "pt")

pcdstringPt :: Parser String
pcdstringPt = choice [lexeme $ show <$> try (rawDoublePt <* (lookAhead $ (void spaceChar) <|> eof)), pcdstring]

optstringPt :: Parser String
optstringPt = choice [lexeme $ show <$> try (rawDoublePt <* (lookAhead $ (void spaceChar) <|> eof)), optstring]

doublePt :: Parser Double
doublePt = read <$> lexeme (rstripPt rawDouble)

double :: Parser Double
double = read <$> lexeme rawDouble

rawDoublePt :: Parser Double
rawDoublePt = read <$> rstripPt rawDouble

rawDouble :: Parser String
rawDouble = (++) <$> ((++) <$> integer <*> (option "" ((:) <$> (char '.') <*> number))) <*> (option "" ((:) <$> (oneOf "eE") <*> integer))
  where integer = plus <|> minus <|> number
        number = some digitChar
        plus = char '+' *> number
        minus = (:) <$> char '-' <*> number

pcdcolor :: Parser PCDColor
pcdcolor = lexeme $ choice [try parseRGB, try parseCMYK, parseCommonColor]

parseRGB :: Parser PCDColor
parseRGB = do
  _ <- symbol "0x"
  RGBA <$> readRGB <*> readRGB <*> readRGB <*> pure 1.0

readRGB :: Parser Double
readRGB = do
  c1 <- letterChar
  c2 <- letterChar
  case readHex [c1, c2] of
    [] -> fail "parsing hex error"
    ((a, _) : _) -> pure (a / 255.0)

parseCMYK :: Parser PCDColor
parseCMYK = do
  _ <- symbol "CMYK:"
  c <- double
  m <- (symbol "," *> double)
  y <- (symbol "," *> double)
  k <- (symbol "," *> double)
  a <- option 1.0 (symbol "," *> double)
  return $ CMYK c m y k a

colorMap :: Map.Map String PCDColor
colorMap = Map.fromList [ ("white", RGBA 1.0 1.0 1.0 1.0)
                        , ("red"  , RGBA 1.0 0.14915693904021 0.00015259021897 1.0)
                        , ("green", RGBA 0.00003051804379 0.97705043106737 0.00378423743038 1.0)
                        , ("blue" , RGBA 0.01542687113756 0.19856565194171 1.0 1.0)
                        , ("cyan", RGBA 0.0 0.99148546578164 1.0 1.0)
                        , ("magenta", RGBA 1.0 0.25275043869688 1.0 1.0)
                        , ("yellow", RGBA 0.98783855954833 0.98535133897917 0.00007629510948 1.0)
                        , ("black", RGBA 0.0 0.0 0.0 1.0)
                        , ("orange", RGBA 1.0 0.57634851606012 0.0 1.0)
                        , ("brown", RGBA 0.66807049668116 0.47512016479744 0.25860990310521 1.0)
                        , ("purple", RGBA 0.5792172121767 0.12805371175708 0.57271686884871 1.0)
                        , ("gray", RGBA 0.57236591134508 0.57233539330129 0.57236591134508 1.0)
                        , ("darkgray", RGBA 0.4078 0.4078 0.4078 1.0)
                        , ("lightgray", RGBA 0.7234 0.7234 0.7234 1.0)
                        , ("clear", RGBA 0.0 0.0 0.0 0.0)
                        , ("none", RGBA 0.0 0.0 0.0 0.0)
                        , ("transparent", RGBA 0.0 0.0 0.0 0.0)

                        , ("50k"          , CMYK 0.00 0.00 0.00 0.50 1.0)
                        , ("cheese"       , CMYK 0.00 0.13 0.90 0.00 1.0)
                        , ("grass"        , CMYK 0.35 0.00 1.00 0.00 1.0)
                        , ("chestnut"     , CMYK 0.00 0.09 0.50 0.24 1.0)
                        , ("darkgreen"    , CMYK 0.32 0.00 1.00 0.79 1.0)
                        , ("olive"        , CMYK 0.27 0.00 0.95 0.55 1.0)
                        , ("christmasred" , CMYK 0.00 0.90 0.86 0.00 1.0)
                        , ("hypo-black"   , CMYK 0.10 0.10 0.10 1.00 1.0)
                        , ("hypo-lightgray" , CMYK 0.10 0.10 0.10 0.60 1.0)
                        , ("hypo-ticketgray", CMYK 0.05 0.05 0.05 0.30 1.0)
                        ]

parseCommonColor :: Parser PCDColor
parseCommonColor = do
  colorName <- lexeme (some $ choice [alphaNumChar, char '-'])
  case Map.lookup colorName colorMap of
    Nothing -> fail "unrecognized color name"
    Just r -> return r
