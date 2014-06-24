{-# LANGUAGE OverloadedStrings #-}

module Morse.Commands.Server
  ( runServer
  , blaze
  ) where

import Morse.Engine
import Morse.Web.Json
import qualified Morse.Web.Views as V

import Control.Applicative ((<$>))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Network.Wai.Parse (parseHttpAccept)
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty


maxValueLength :: Int64
maxValueLength = 2048

runServer :: Int -> IO ()
runServer port = scotty port $ do
  middleware $ staticPolicy (noDots >-> addBase "static")
  middleware logStdoutDev

  get "/" $ blaze (V.index Nothing Nothing)

  post "/" $ do
    val <- fromMaybe TL.empty <$> maybeParam "val"
    case (detectProcessType . TL.unpack) val of
      PTDecode -> redirect ("/decode?val=" `TL.append` val)
      PTEncode -> redirect ("/encode?val=" `TL.append` val)

  get "/encode" $ process toMorse

  get "/decode" $ process fromMorse

  notFound $ formatResponse $ \fmt -> case fmt of
    RFHtml -> blaze V.notFound
    RFJson -> json $ MsgRes "not found"

process :: (String -> String) -> ActionM ()
process f = do
  val <- maybe T.empty (TL.toStrict . TL.take maxValueLength) <$> maybeParam "val"
  let res = (T.pack . f . T.unpack) val
  formatResponse $ \fmt -> case fmt of
    RFHtml -> blaze (V.index (Just val) (Just res))
    RFJson -> json $ MorseRes val res


-------------------------------------------------------------------------------
-- utils
-------------------------------------------------------------------------------

maybeParam :: Parsable a => TL.Text -> ActionM (Maybe a)
maybeParam name = flip rescue (const $ return Nothing) (param name >>= return . Just)

blaze :: Html -> ActionM ()
blaze = html . renderHtml

data ResponseFormat
  = RFHtml
  | RFJson

formatResponse :: (ResponseFormat -> ActionM ()) -> ActionM ()
formatResponse f = do
  mAccept <- header "Accept"
  let mAccepts = (parseHttpAccept . TE.encodeUtf8 . TL.toStrict) <$> mAccept
  case mAccepts of
    Just xs | "text/html" `elem` xs -> f RFHtml
            | "application/json" `elem` xs -> f RFJson
    _ -> f RFHtml