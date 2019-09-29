module Main where

import Control.Monad
import Control.Monad.Trans
import Data.Time.Clock
import Data.Time.Format
import Text.Printf
import System.IO
import System.Random
import Happstack.Server
import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

page = H.html $ do
  H.head $ H.toHtml ""
  H.body $ do
    H.form ! A.method (H.stringValue "post") $ do
      H.textarea ! A.name (H.stringValue "text") $ H.toHtml ""
      H.br
      H.input ! A.type_ (H.stringValue "submit")

mylogger h u t r c s rf ua = do
  let ft = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" t
  appendFile "log.txt" $
    ft++"\n "++h++":"++u++"\n "++r++"\n "++show c
     ++":"++show s++"\n ref: "++rf++"\n ua: "++ua++"\n"

main :: IO ()
main = do
  putStrLn "server starting..."
  simpleHTTP nullConf { logAccess = Just mylogger } $ do
    -- setHeaderM "Server" "test"
    composeFilter contentLength
    msum
      [ dir "t" $ msum
        [ method GET >> ok (toResponse page)
        , method POST >> do
            decodeBody $ defaultBodyPolicy "" 100 10000 100
            x <- look "text"
            lift $ do
              t <- getCurrentTime
              let ft = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" t
              rn <- fmap (printf "%08x" :: Int -> String) $ getStdRandom $ randomR (0, 2^32-1)
              let fn = "file-" ++ ft ++ "-" ++ rn
              writeFile fn x
            ok (toResponse "ok")
        ]
      , ok $ toResponse "hello"
      ]
