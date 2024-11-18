
-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}

module RenderUtil ( renderHead, formUrlEncodeQuery ) where

import qualified Lucid as L
import Relude hiding (ord)
import Text.RawString.QQ

import qualified Data.Char as Char
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-- import           Network.HTTP.Types


formUrlEncodeQuery :: [(String, String)] -> LB.ByteString
formUrlEncodeQuery = Builder.toLazyByteString . mconcat . intersperse amp . map encodePair
  where
    equals = Builder.word8 (ord '=')
    amp = Builder.word8 (ord '&')
    percent = Builder.word8 (ord '%')
    plus = Builder.word8 (ord '+')

    encodePair :: (String, String) -> Builder
    encodePair (key, value) = encode key <> equals <> encode value

    encode :: String -> Builder
    encode = escape . T.encodeUtf8 . T.pack . newlineNormalize

    newlineNormalize :: String -> String
    newlineNormalize input = case input of
      [] -> []
      '\n' : xs -> '\r' : '\n': newlineNormalize xs
      x : xs -> x : newlineNormalize xs

    escape :: ByteString -> Builder
    escape = mconcat . map f . B.unpack
      where
        f :: Word8 -> Builder
        f c
          | p c = Builder.word8 c
          | c == ord ' ' = plus
          | otherwise = percentEncode c

        p :: Word8 -> Bool
        p c =
             ord 'a' <= c && c <= ord 'z'
          || c == ord '_'
          || c == ord '*'
          || c == ord '-'
          || c == ord '.'
          || ord '0' <= c && c <= ord '9'
          || ord 'A' <= c && c <= ord 'Z'

    ord :: Char -> Word8
    ord = fromIntegral . Char.ord

    percentEncode :: Word8 -> Builder
    percentEncode n = percent <> hex hi <> hex lo
      where
        (hi, lo) = n `divMod` 16

    hex :: Word8 -> Builder
    hex n = Builder.word8 (offset + n)
      where
        offset
          | n < 10    = 48
          | otherwise = 55

renderHead :: Text -> L.Html ()
renderHead t =
  L.head_ $ do
    L.title_ $ L.toHtml t
    L.meta_ [L.charset_ "utf-8"]
    L.meta_ [L.name_ "viewport", L.content_ "width=device-width, initial-scale=1.0"]
    L.meta_ [L.httpEquiv_ "X-UA-Compatible", L.content_ "ie=edge"]
    let ttt :: Text; ttt = ""
    L.script_ [L.src_ "https://kit.fontawesome.com/dd23371146.js", L.crossorigin_ "anonymous"] ttt
    -- <script src="https://kit.fontawesome.com/dd23371146.js" crossorigin="anonymous"></script>
    L.style_ styleqq

styleqq :: Text
styleqq =
  [r|

@font-face {
   font-family: sansation;
   src: url(sansation_light.woff);
}

body {
   font-family: "HelveticaNeue-Light", "Helvetica Neue Light", "Helvetica Neue", Helvetica, Arial, "Lucida Grande", sans-serif; 
   font-weight: 300;
}

p {
  margin: 0 0 0 0;
}

ul {
  list-style-type: none;
  margin: 0;
  padding: 0;
  width: 12%;
  background-color: #f1f1f1;
  height: 100%; /* Full height */
  position: fixed; /* Make it stick, even on scroll */
  overflow: auto; /* Enable scrolling if the sidenav has too much content */
}

li a {
  display: block;
  color: #000;
  padding: 8px 16px;
  text-decoration: none;
}

li a:hover {
  background-color: #555;
  color: white;
}

.active {
  background-color: #333;
  color: white;
}

#navbar {
  overflow: visible;
  position: fixed;
  top: 0; /* stay on top */
  width: 100%;
  transition: top 0.6s; /* Transition effect when sliding down (and up) */
  background-color: #ff7000;
}

#navbar a {
  float: left;
  font-size: 16px;
  color: white;
  text-align: center;
  padding: 14px 16px;
  text-decoration: none;
}

#navbar .dropdown a {
  font-size: 16px;
  color: white;
  padding: 14px 2px;
  text-decoration: none;
}

#navbar .dropdown a i {
  padding: 2px 0px;
}
#navbar .dropdown {
  float: left;
  overflow: visible;
  position: relative;
}

#navbar .dropdown .dropbtn {
  padding: 14px 16px 14px 16px;
  font-size: 16px;
  border: none;
  outline: none;
  color: white;
  background-color: inherit;
  font-family: inherit;
  margin: 0;
}

#navbar .dropdown .dropbtn a {
  padding: 0px 4px;
}
#navbar .dropdown .dropbtn-order {
  padding: 14px 16px 14px 2px;
  font-size: 16px;
  border: none;
  outline: none;
  color: white;
  background-color: inherit;
  font-family: inherit;
  margin: 0;
}
#navbar .dropdown .dropbtn-order a {
  padding: 0px 4px;
}

#navbar a:hover, #navbar .dropdown:hover .dropbtn {
  background-color: red;
}

#navbar .dropdown .dropdown-content {
  display: none;
  position: absolute;
  color: black;
  background-color: #f9f9f9;
  padding: 5px 8px;
  margin: 1px 0px;
  border-radius: 6px;
  min-width: 160px;
  max-width: 600px;
  max-height: 600px;
  overflow: auto;
  box-shadow: 0px 8px 16px 0px rgba(0,0,0,0.2);
  z-index: 1;
}

#navbar .dropdown .dropdown-content a {
  float: none;
  color: black;
  padding: 5px 5px;
  margin: 0px 0px;
  border-radius: 6px;
  text-decoration: none;
  display: block;
  text-align: left;
}

#navbar .dropdown .dropdown-content a:hover {
  background-color: orange;
}

#navbar .dropdown:hover .dropdown-content {
  display: grid;
}

#navbar .dropdown .focus-content {
  display: none;
/*  display: block; */
  position: absolute;
  color: black;
  background-color: #f9f9f9;
  padding: 5px 8px;
  margin: 1px 0px;
  border-radius: 6px;
  min-width: 800px;
  max-height: 600px;
  overflow: auto;
  box-shadow: 0px 8px 16px 0px rgba(0,0,0,0.2);
  z-index: 1;
}

#navbar .dropdown:hover .focus-content {
  display: block;
}

#navbar .dropdown .focus-content a {
  display: inline;
  float: none;
  color: black;
  padding: 5px 5px;
  margin: 0px 0px;
  border-radius: 6px;
  text-decoration: none;
  text-align: left;
}

.focus-on {
  display: inline;
  float: none;
  color: black;
  background-color: green;
  padding: 5px 5px;
  margin: 0px 0px;
  border-radius: 6px;
  text-decoration: none;
  text-align: left;
}

.focus-not {
  display: inline;
  float: none;
  color: black;
  background-color: red;
  padding: 5px 5px;
  margin: 0px 0px;
  border-radius: 6px;
  text-decoration: none;
  text-align: left;
}

#navbar .dropdown .focus-content a:hover {
  background-color: orange;
}


/**************************************************************/
* {
  box-sizing: border-box;
}

img {
/*  border: 1px solid #ddd; */
  border-radius: 4px;
  padding: 5px;
}

img:hover {
  box-shadow: 0 0 2px 1px rgba(0, 140, 186, 0.5);
}

.album-thumb {
  padding: 0px 1px 10px;
}

.album-info {
  width: 210px;
}

p.album-title {
  white-space: nowrap; 
  /* border: 1px solid #ddd; */
  overflow: hidden;
  text-overflow: ellipsis;
  font-family: helvetica;
  font-size: 14px;
  margin: 2px 0 0 0;
}
p.album-artist {
  white-space: nowrap; 
  /* border: 1px solid #ddd; */
  overflow: hidden;
  text-overflow: ellipsis;
  font-family: helvetica;
  font-size: 11px;
  margin: 4px 0 0 0;
}

.albums12perc {
  margin-left:12%;
  padding:1px 16px;
/*  height:1000px; */
 }

.albums {
   padding:15px 15px 250px;
   margin-top:50px;
 }

.row {
  display: flex;
  justify-content: space-evenly;
  flex-wrap: wrap;
  padding: 0 1px;
}

/* Container needed to position the overlay. Adjust the width as needed */
.cover-container {
  width:  205px;
  height: 205px;
  position: relative;
  text-align: center;
  vertical-align: middle;
}
.cover-container:hover {
  box-shadow: 0 0 2px 1px rgba(0, 140, 186, 0.5);
}

/* Make the image to responsive */
.cover-image {
  display: block;
  width: 100%;
  height: 100%;
  border-radius: 4px;
  padding: 5px;
  position: absolute;
  top: 0;
  left: 0;
}
.cover-image:hover {
  box-shadow: 0 0 2px 1px rgba(0, 140, 186, 0.5);
}

.cover-obackground {
  width: 16px;
  height: 16px;
  padding-top: 1px;
  border-radius: 4px;
  position: absolute;
  right: 7;
  bottom: 7;
  background-color: rgba(255,255,255,.5);
}
.cover-obackground a:link { color: black; }
.cover-obackground a:visited { color: black; }
.cover-obackground a:hover { color: red; }
.cover-obackground .hovtext {
  visibility: hidden;
  width: 120px;
  padding-top: 1px;
  background-color: black;
  color: #777;
  padding: 5px 0;
  border-radius: 6px;
  position: absolute;
  z-index: 1;
}
.cover-obackground:hover .hovtext {
  visibility: visible;
}

.cover-oimage {
  display: block;
  width: 16px;
  height: 16px;
  padding: 0px;
}
a:link {
  color: black;
}
a:visited {
  color: black;
}
a:hover {
  color: rgba(0, 140, 186, 0.5);
}
a:active {
  color: blue;
}

.rat {
  width: 70px;
  height: 14px;
  font-size:small;
  color: gold;
  padding-top: 1px;
  border-radius: 4px;
  position: absolute;
  left: 87;
  bottom: 7;
  background-color: rgba(255,255,255,.5);
}
.rat0 {
  width: 70px;
  height: 13px;
  font-size:small;
  color: black;
  padding: 0px;
  border-radius: 4px;
  position: absolute;
  left: 87;
  bottom: 7;
}
.plays {
  width: 15px;
  height: 14px;
  font-size:small;
  color: black;
  padding-top: 1px;
  border-radius: 4px;
  position: absolute;
  left: 71;
  bottom: 7;
  background-color: rgba(255,255,255,.5);
}
.plays .hovtext {
  visibility: hidden;
  width: 120px;
  padding-top: 1px;
  background-color: black;
  color: #777;
  padding: 5px 0;
  border-radius: 6px;
  position: absolute;
  z-index: 1;
}
.plays:hover .hovtext {
  visibility: visible;
}

.idx {
  height: 18px;
  padding-left: 4px;
  padding-right: 4px;
  text-align: left;
  position: absolute;
  top: 1;
  left: 1;
  display: inline-block;
  border-radius: 9px;
  background-color: rgba(155,155,155,.5);
}

.loc {
  height: 16px;
  position: relative;
  display: inline-block;
  border-radius: 8px;
  background-color: rgba(155,155,155,.5);
}

.loc .hovtext {
  visibility: hidden;
  width: 120px;
  padding-top: 1px;
  background-color: black;
  color: #777;
  padding: 5px 0;
  border-radius: 6px;
  position: absolute;
  z-index: 1;
}

.loc:hover .hovtext {
  visibility: visible;
}
.loclink {
  color: #0ff;
}


.cover-obackground1 {
  width: 16px;
  height: 16px;
  border-radius: 4px;
  border-radius: 4px;
  position: absolute;
  left: 7;
  bottom: 7;
  background-color: rgba(255,255,255,.5);
}

.cover-obackground3 {
  width: 16px;
  height: 16px;
  border-radius: 4px;
  border-radius: 4px;
  position: absolute;
  left: 27;
  bottom: 7;
  background-color: rgba(255,255,255,.5);
}

.cover-obackground2 {
  height: 16px;
  width: 20px;
  color: black;
  border-radius: 4px;
  position: absolute;
  left: 47;
  bottom: 7;
  background-color: rgba(255,255,255,.5);
}

.cover-obackground2 a:link { color: black; }
.cover-obackground2 a:visited { color: black; }
.hovtext a:link { color: white; }
.hovtext a:visited { color: white; }

.cover-obackground2 .hovtext {
  visibility: hidden;
  width: 120px;
  background-color: black;
  color: #fff;
  padding: 5px 0;
  border-radius: 6px;
  position: absolute;
  z-index: 1;
}

.cover-obackground2:hover .hovtext {
  visibility: visible;
}

|]
