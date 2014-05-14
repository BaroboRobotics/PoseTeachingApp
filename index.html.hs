{-# LANGUAGE OverloadedStrings #-}
-- vim: sw=2

import Prelude hiding (div, span)
import qualified Prelude as P
import Data.Monoid (mempty)

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (label, form, span)
import qualified Text.Blaze.Html5.Attributes as A

import Text.Blaze.Html.Renderer.Pretty (renderHtml)

import Angular

import System.IO.Unsafe (unsafePerformIO)


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
      js "linkbot.js"
      js "bower_components/angular/angular.js"
      js "poseTeach.js"
      css "bower_components/bootstrap/dist/css/bootstrap.css"
    body ! ngApp "" $ div ! ngController "actions" $ do
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

robotManager = do
  activeRobotDisplay
  robotForm $ do
    "form-group" .$ do
      roboInputLabel
      roboInput
    connectBtn
    roboIndicator

  where
  activeRobotDisplay = "active-robot" .! ngShow "m.roboId !== null" $ do
    "{{m.roboId}}"
    span !. "active-robot--status" $ "{{m.roboStatus}}"
  robotForm = form !. "sidebar--robot-mgr form-inline"
  roboInputLabel = label !. "sr-only" ! for "roboInput" $ "Linkbot ID"
  roboInput = input ! ngModel "m.robotId" !. "form-control" ! type_ "text" ! placeholder "Linkbot ID"
  connectBtn = button !. "form-control" ! ngClick "connect()" $ "+"
  roboIndicator = "-"


programListingSection =
  section !. "program-listing container" $ do
    programControls
    programCode

programControls =
  "program-controls" .$ do
    button "Run"
    a ! ngClick "clearProgram()" $ "Clear"

programCode =
  pre !. "program-code" $ do
    "program-code--boilerplate" .$
      pythonBoilerplate
    "program-code--code" .! ngRepeat "pose in m.poses" $ do
      "# Pose {{$index+1}}"
      "linkbot.moveTo({{pose[0]}}, {{pose[1]}}, {{pose[2]}})"

pythonBoilerplate = unsafeTmpl "boilerplate.py"

unsafeTmpl = str . ("\n" ++) . unsafePerformIO . readFile
