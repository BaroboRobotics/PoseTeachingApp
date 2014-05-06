{-# LANGUAGE OverloadedStrings #-}
-- vim: sw=2

import Prelude
import qualified Prelude as P
import Data.Monoid (mempty)

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

import Text.Blaze.Html.Renderer.Pretty (renderHtml)

import Angular

elem !. c = elem ! class_ c
elem !# i = elem ! A.id i

js uri = script ! src uri $ mempty
css uri = link ! rel "stylesheet" ! href uri

klass .$ innerHtml = H.div !. klass $ innerHtml
klass .! attr      = H.div !. klass ! attr

-- | Coerce that squirrely string literal
str :: String -> Html
str = toHtml

val :: String -> AttributeValue
val = toValue
-- -

main = putStrLn $ renderHtml $ do
  docType
  html ! lang "en" $ do
    H.head $ do
      H.title $ "Pose Teaching"
      meta ! httpEquiv "Content-Type" ! content "text/html; charset=utf-8"
    body $ do
      adminSidebar
      programListingSection

adminSidebar =
  "sidebar" .$ do
    lllogo
    appTitle
    robotManager

lllogo =
  img !. "sidebar--logo"
      ! src "linkbot-labs-ER-logo-200x46px.png"

appTitle =
  h1 !. "sidebar--title" $ "Pose Teaching"

robotManager =
  "sidebar--robot-mgr" .$ do
    "Put robots here thx"

programListingSection =
  section !. "program-listing" $ do
    programControls
    programCode

programControls =
  "program-controls" .$ do
    "Run the code, or not"

programCode =
  "program-code" .$ do
    "program-code--boilerplate" .$
      pythonBoilerplate
    "program-code--code" .$
      codeLines

pythonBoilerplate = "# blah blah"
codeLines = "robot.move(stuff)"
