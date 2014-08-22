module Main where

import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import Data.Word8
import qualified Database.Redis as R
import Network.Mail.Mime (randomString)
import Network.URI (parseURI)
import qualified System.Random as SR
import Web.Scotty

shortyGen :: SR.RandomGen b => b -> BS.ByteString
shortyGen = encodeUtf8 . TL.toStrict . TL.pack . fst . randomString 7

saveURI conn shortURI uri = R.runRedis conn $ R.set shortURI uri
getURI conn shortURI = R.runRedis conn $ R.get shortURI

main = scotty 3000 $ do
  rConn <- liftIO (R.connect R.defaultConnectInfo)
  get "/" $ do
    uri <- param "uri"
    case parseURI (TL.unpack uri) of
      Just _  -> do
        g <- liftIO SR.getStdGen
        let shorty = shortyGen g
        resp <- liftIO (saveURI rConn shorty (encodeUtf8 (TL.toStrict uri)))
        text $ TL.concat [(TL.pack (show resp)), " shorty is: ", TL.fromStrict (decodeUtf8 shorty)]
      Nothing -> text (TL.concat [uri, " wasn't a url"])
  get "/:short" $ do
    short <- param "short"
    uri <- liftIO (getURI rConn short)
    case uri of
      Left reply -> text (TL.pack (show reply))
      Right mbBS -> case mbBS of
        Nothing -> text "uri not found"
        Just bs -> html $ TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]
          where tbs = TL.fromStrict (decodeUtf8 bs)
