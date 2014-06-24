{-# LANGUAGE OverloadedStrings #-}

module Morse.Web.Views 
  ( index
  , notFound
  ) where

import qualified Data.ByteString.Char8 as C8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze (customAttribute)
import Text.Blaze.Html5 ((!), Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (MarkupM(Empty, Parent), StaticString(..))


layout :: Text -- ^ title
       -> Html -- ^ content
       -> Html
layout ttl content = do
  H.docType
  comment "lt IE 7" $ htmlEn ! A.class_ "ie ie6"
  comment "IE 7" $ htmlEn ! A.class_ "ie ie7"
  comment "IE 8" $ htmlEn ! A.class_ "ie ie8"
  comment "(gte IE 9)|!(IE)" $ htmlEn
  H.head $ do
    H.meta ! (customAttribute "charset" "utf-8")
    H.title (H.toHtml $ "Morse Code Encoder/Decoder - " `T.append` ttl)
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1, maximum-scale=1"
    H.link ! A.rel "stylesheet" ! A.href "/css/base.css"
    H.link ! A.rel "stylesheet" ! A.href "/css/skeleton.css"
    H.link ! A.rel "stylesheet" ! A.href "/css/layout.css"
  htmlBody $ do
    H.div ! A.class_ "container" $ do
      H.div ! A.class_ "sixteen columns" $ content
      H.footer ! A.class_ "sixteen columns footer" $ do
        H.span "Created by RJ Regenold."
        H.a ! A.href "https://github.com/rjregenold/morse" $ "Source"
  where htmlEn = html' ! A.lang "en" $ ""

index :: Maybe Text -- ^ value to be encoded/decoded 
      -> Maybe Text -- ^ encoded/decoded result
      -> Html
index mVal mRes = layout "Home" $ H.div $ do
  H.h1 $ do
    H.a ! A.href "/" $ "Morse Code Encoder/Decoder"
  fromMaybe Empty $ flip fmap mRes $ \res -> do
    H.div ! A.class_ "result-container" $ do
      H.strong "Result"
      H.pre $ H.toHtml res
  H.form ! A.action "/" ! A.method "POST" $ do
    H.textarea ! A.name "val" ! A.placeholder "Enter text to encode/decode." $ (H.toHtml $ fromMaybe T.empty mVal)
    H.div ! A.class_ "form-actions" $ do
      H.input ! A.type_ "submit" ! A.value "Process"

notFound :: Html
notFound = layout "Not found" $ H.div $ do
  H.h1 "Not found"
  H.p "Sorry, what you are looking for is not here."


-------------------------------------------------------------------------------
-- utils
-------------------------------------------------------------------------------

-- | HTML tag with no closing.
html' :: Html -> Html
html' = Parent "html" "<html" ""
 
-- | Special tag that opens only body but closes both body and html.
htmlBody :: Html -> Html
htmlBody = Parent "body" "<body" "</body></html>"

-- | Comment combinator. `if` is included, so only condition is needed.
comment :: String -> Html -> Html
comment cond = Parent "comment" (ss cond') "<![endif]-->"
  where cond' = "<!--[if " ++ cond ++ " ]"
        ss s = StaticString (s ++) (C8.pack s) (T.pack s)
