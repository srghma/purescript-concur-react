module Test.Keyboard where

import Prelude

import Concur.Core.Types (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Data.BooleanAlgebra (not)
import Data.Eq (class Eq, (==))
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Show (class Show, show)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import FRP.Event (Event)
import FRP.Event as FRP.Event
import React.SyntheticEvent as R
import Web.Event.Event (EventType(..))
import Web.Event.Event as Web.Event.Event
import Web.Event.EventTarget as Web.Event.EventTarget
import Web.HTML as Web.HTML
import Web.HTML.Window as Web.HTML.Window
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as Web.UIEvent.KeyboardEvent
import Web.DOM.Document as Web.DOM.Document
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument
import Effect.AVar as Effect.AVar
import Effect.Aff.AVar as Effect.Aff.AVar


-- A virtual keyboard, that also demonstrates how to handle document level events


-- Main Widget ----------------------------------------------------

-- A never-ending virtual keypad widget.
-- Allows the user to navigate and select a key. Displays the selected key.
keypadWidget :: forall a. Widget HTML a
keypadWidget = do
  keyboardRef <- liftEffect $ do
    keyboardRef <- Effect.AVar.empty

    -- TODO: don't silently fail to put, use Channel instead of EVar
    stopListeningKeydownEvent <- FRP.Event.subscribe documentKeydownEvent \keyboardEvent -> void $ Effect.AVar.tryPut keyboardEvent keyboardRef

    pure keyboardRef

  let (awaitKeyboardEvent :: Aff KeyboardEvent) = Effect.Aff.AVar.take keyboardRef

  go awaitKeyboardEvent Enter ""

  where
  go awaitKeyboardEvent focus msg = do
    keyPressed <- virtualKeyInput awaitKeyboardEvent focus <|> D.div' [D.text msg]
    go awaitKeyboardEvent keyPressed ("You clicked: " <> show keyPressed)

-- | -- On off button for key events
-- | toggleEvents :: forall a. Widget HTML a
-- | toggleEvents = go false
-- |   where
-- |   go enabled = do
-- |     _ <- D.button [P.onClick] [D.text $ if enabled then "stop listening" else "start listening"]
-- |     liftEffect (if enabled then stopListening else startListening)
-- |     go (not enabled)

-- Displays a keypad with the supplied initial focus.
-- Allows the user to navigate and select a key. Returns the selected key.
virtualKeyInput :: Aff KeyboardEvent -> Focus -> Widget HTML Key
virtualKeyInput awaitKeyboardEvent focus = do
  (key :: Maybe Key) <- liftAff (map toKey awaitKeyboardEvent) <|> keypadButtons focus
  case key of
    Just Enter -> pure focus
    Nothing -> virtualKeyInput awaitKeyboardEvent focus
    Just ArrowUp -> virtualKeyInput awaitKeyboardEvent (transition focus U)
    Just ArrowDown -> virtualKeyInput awaitKeyboardEvent (transition focus D)
    Just ArrowLeft -> virtualKeyInput awaitKeyboardEvent (transition focus L)
    Just ArrowRight -> virtualKeyInput awaitKeyboardEvent (transition focus R)

-- Dispay only. Renders the keypad buttons with the supplied focus
keypadButtons :: forall a. Focus -> Widget HTML a
keypadButtons focus = D.table' $ pure $ D.tbody'
  [ D.tr' [ blank,         but ArrowUp,   blank          ]
  , D.tr' [ but ArrowLeft, but Enter,     but ArrowRight ]
  , D.tr' [ blank,         but ArrowDown, blank          ]
  ]
  where
    blank = D.td' []
    but key = D.td [style key] [D.text (show key)]
    spanstyle = P.style
      { verticalAlign: "middle"
      }
    style key = P.style
      { width: "50px"
      , height: "40px"
      , background: if key==focus then "lightblue" else "gray"
      , textAlign: "center"
      }


-- FFI ------------------------------------------------------------

documentKeydownEvent :: Event KeyboardEvent
documentKeydownEvent = FRP.Event.makeEvent \push -> do
  window <- Web.HTML.window

  document <- Web.HTML.Window.document window

  eventListener <- Web.Event.EventTarget.eventListener
    (\event ->
      case Web.UIEvent.KeyboardEvent.fromEvent event of
          Nothing -> pure unit
          Just event' -> push event'
    )

  let eventType = EventType "keydown"

  Web.Event.EventTarget.addEventListener eventType eventListener false (Web.HTML.HTMLDocument.toEventTarget document)

  pure $ Web.Event.EventTarget.removeEventListener eventType eventListener false (Web.HTML.HTMLDocument.toEventTarget document)

-- Data structures ------------------------------------------------

data Key
  = ArrowUp
  | ArrowDown
  | ArrowLeft
  | ArrowRight
  | Enter

instance showKey :: Show Key where
  show ArrowUp = "Up"
  show ArrowDown = "Down"
  show ArrowLeft = "Left"
  show ArrowRight = "Right"
  show Enter = "Enter"

instance eqKey :: Eq Key where
  eq ArrowUp ArrowUp = true
  eq ArrowDown ArrowDown = true
  eq ArrowLeft ArrowLeft = true
  eq ArrowRight ArrowRight = true
  eq Enter Enter = true
  eq _ _ = false

type Focus = Key

data Dir = U | D | L | R

toKey :: KeyboardEvent -> Maybe Key
toKey event =
  case Web.UIEvent.KeyboardEvent.key event of
    "ArrowUp" -> Just ArrowUp
    "ArrowDown" -> Just ArrowDown
    "ArrowLeft" -> Just ArrowLeft
    "ArrowRight" -> Just ArrowRight
    "Enter" -> Just Enter
    _ -> Nothing

transition :: Key -> Dir -> Key
transition ArrowRight L = Enter
transition Enter L = ArrowLeft
transition ArrowLeft R = Enter
transition Enter R = ArrowRight
transition ArrowUp D = Enter
transition Enter D = ArrowDown
transition ArrowDown U = Enter
transition Enter U = ArrowUp
transition k _ = k
