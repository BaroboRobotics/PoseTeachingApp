{-# LANGUAGE OverloadedStrings #-}
-- vim: sw=2

import Prelude hiding (div, span)
import qualified Prelude as P
import Data.Monoid (mempty)
import qualified Data.Text as T

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (label, form, span)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Internal as Internal

-- The Angular module provides some combinators for using/adding Angular
-- directives. Source at https://github.com/chreekat/blaze-angular .
import Angular

-- These are some generic Blaze combinators: mostly shortcuts for adding
-- classes or ids to elements, but also a few other generic shortcuts.
type HtmlCombr = Html -> Html

(!.), (!#) :: Internal.Attributable h => h-> AttributeValue -> h
elem !. klass = elem ! class_ klass
elem !# id_   = elem ! A.id id_

(.!) :: AttributeValue -> Attribute -> HtmlCombr
klass .! attr  = H.div !. klass ! attr

(.$) :: AttributeValue -> HtmlCombr
klass .$ innerHtml = H.div !. klass $ innerHtml

js, css :: AttributeValue -> Html
js uri = script ! src uri $ mempty
css uri = link ! rel "stylesheet" ! href uri


-- These create an element and attribute for the "modifiable" directive.
-- See poseTeach.coffee to learn about directives.
modifiable :: Html -> Html
modifiable = elemDirective "modifiable"

modData :: AttributeValue -> Attribute
modData = customAttribute "mod-data"

-- I only use 'modifiable' in one particular way, so I encapsulate that
-- here:
modifiableNumber
  :: String -- ^ The Angular scope variable that will be tracked/modified
  -> Html
modifiableNumber dataRef =
  modifiable ! customAttribute "number" "true"
             ! modData (toValue dataRef) $
      span ! A.style "border-bottom: 1px dotted black" $
        toHtml $ template $ T.pack $ dataRef ++ " |number:1"
-- ...and then make an easy-to-use synonym
modNum = modifiableNumber

-- Kind of a hacky way to generate a "code line": just append a <br>
codeLn :: HtmlCombr
codeLn = (>> br)

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
    body ! ngApp "PoseTeaching" ! ngController "actions" $ do
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
  robotForm $ do
    "form-group" .$ do
      roboInputLabel
      roboInput
    connectBtn
  activeRobotDisplay

  where
  activeRobotDisplay = "active-robot" .! ngRepeat "robot in m.robots" $ do
                         "{{robot._id |uppercase}}"
  robotForm = form !. "sidebar--robot-mgr form-inline"
  roboInputLabel = label !. "sr-only" ! for "roboInput" $ "Linkbot ID"
  roboInput = input ! ngModel "m.robotIdInput" !. "form-control"
                    ! type_ "text"
                    ! placeholder "Linkbot ID"
  connectBtn = button !. "form-control" ! ngClick "connect()" $ "+"


programListingSection =
  section !. "program-listing container" $ do
    programControls
    programCode

programControls =
  "program-controls" .$ do
    button ! ngClick "toggleRun()" $ do
      "{{ m.moveStatus.running() ? 'Pause' : 'Run' }}"
    button ! ngClick "m.loop = m.loop ? false : true" $ "Loop?"
    a ! ngClick "clearProgram()" $ "Clear"

programCode =
  pre !. "program-code" $ do
    "program-code--boilerplate" .$
      pythonBoilerplate
    "program-code--code" .! ngIf "m.poses.length > 0" $ do
      codeLn "while True:"
      div ! ngRepeat "pose in m.poses" $ do
        codeLn ""
        codeLn $ do
          "    # Pose {{$index+1}} "
          span ! ngIf "m.moveStatus.timeout && $index == m.moveStatus.index" $
            "<-- moving here"
          span ! ngIf "! m.moveStatus.timeout && $index == m.moveStatus.index" $
            " -!- last position"
        div ! ngIf "m.robots.length > 1" $ do
          div ! ngRepeat "r in m.robots" $ do
            -- Generates e.g. "linkbot1.moveToNB(80.2, 28.9, 91.3)"
            codeLn $ do
              "    linkbot{{$index+1}}.moveToNB("
              modNum "pose[$index][0]" >> ", "
              modNum "pose[$index][1]" >> ", "
              modNum "pose[$index][2]" >> ")"
          div ! ngRepeat "r in m.robots" $ do
            codeLn $ do
              "    linkbot{{$index+1}}.moveWait()"
        div ! ngIf "m.robots.length == 1" $ do
          div ! ngRepeat "r in m.robots" $ do
            codeLn $ do
              "    linkbot{{$index+1}}.moveTo("
              modNum "pose[$index][0]" >> ", "
              modNum "pose[$index][1]" >> ", "
              modNum "pose[$index][2]" >> ")"
      div ! ngIf "! m.loop" $ do
        codeLn ""
        codeLn "    break"

pythonBoilerplate = do
  dongleBoilerplate
  linkbotConnections
  setSpeeds

dongleBoilerplate = mapM_ codeLn
  [ "#!/usr/bin/env python"
  , ""
  , "# This file generated by Linkbot Labs Pose Teaching"
  , ""
  , "import barobo"
  , "dongle = barobo.Dongle()"
  , "dongle.connect()"
  , ""
  ]

linkbotConnections = do
  div ! ngRepeat "r in m.robots" $ do
    codeLn "linkbot{{$index+1}} = dongle.getLinkbot('{{r._id | uppercase}}')"
  codeLn ""

setSpeeds = do
  div ! ngRepeat "r in m.robots" $ do
    codeLn $ do
      "linkbot{{$index+1}}.setJointSpeeds("
      modNum "m.speeds[$index][0]" >> ", "
      modNum "m.speeds[$index][1]" >> ", "
      modNum "m.speeds[$index][2]" >> ")"
  codeLn ""
