module Arkham.Types.Event.Cards.ThePaintedWorld
  ( thePaintedWorld
  , ThePaintedWorld(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window
import System.IO.Unsafe

newtype ThePaintedWorld = ThePaintedWorld EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePaintedWorld :: EventCard ThePaintedWorld
thePaintedWorld = event ThePaintedWorld Cards.thePaintedWorld

-- because we are checking if cards are playable will we inevitably
-- trigger a case where we need to check for actions. For example
-- "I'm outta here" needs to check for a resign ability. Doing this
-- will cause infinite recursion, so we take out a global lock to
-- ensure we only run this code once in an iteration
thePaintedWorldRecursionLock :: IORef Bool
thePaintedWorldRecursionLock = unsafePerformIO $ newIORef False
{-# NOINLINE thePaintedWorldRecursionLock #-}

instance
  ( HasList UnderneathCard env InvestigatorId
  , CanCheckPlayable env
  )
  => HasActions env ThePaintedWorld where
  getActions iid (InHandWindow ownerId window) (ThePaintedWorld attrs)
    | iid == ownerId = do
      locked <- readIORef thePaintedWorldRecursionLock
      if locked
        then pure []
        else do
          writeIORef thePaintedWorldRecursionLock True
          underneathCards <- map unUnderneathCard <$> getList iid
          let
            validCards = filter
              (and . sequence
                [(== EventType) . toCardType, not . cdExceptional . toCardDef]
              )
              underneathCards
          playableCards <- filterM (getIsPlayable ownerId [window]) validCards
          writeIORef thePaintedWorldRecursionLock False
          pure
            [ InitiatePlayCardAsChoose
                iid
                (toCardId attrs)
                playableCards
                [ CreateEffect
                    "03012"
                    Nothing
                    (CardIdSource $ toCardId attrs)
                    (CardIdTarget $ toCardId attrs)
                ]
                True
            | notNull playableCards
            ]
  getActions iid window (ThePaintedWorld attrs) = getActions iid window attrs

instance HasModifiersFor env ThePaintedWorld

instance RunMessage env ThePaintedWorld where
  runMessage msg (ThePaintedWorld attrs) =
    ThePaintedWorld <$> runMessage msg attrs
