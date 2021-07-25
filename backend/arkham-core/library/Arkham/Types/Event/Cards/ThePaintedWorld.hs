module Arkham.Types.Event.Cards.ThePaintedWorld
  ( thePaintedWorld
  , ThePaintedWorld(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Window

newtype ThePaintedWorld = ThePaintedWorld EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePaintedWorld :: EventCard ThePaintedWorld
thePaintedWorld = event ThePaintedWorld Cards.thePaintedWorld

instance HasList UnderneathCard env InvestigatorId => HasActions env ThePaintedWorld where
  getActions iid (InHandWindow ownerId (DuringTurn You)) (ThePaintedWorld attrs)
    | iid == ownerId = do
      underneathCards <- map unUnderneathCard <$> getList iid
      let
        validCards = filter
          (and . sequence
            [(== EventType) . toCardType, not . cdExceptional . toCardDef]
          )
          underneathCards
      pure
        [ InitiatePlayCardAsChoose
            iid
            (toCardId attrs)
            validCards
            [ CreateEffect
                "03012"
                Nothing
                (CardIdSource $ toCardId attrs)
                (CardIdTarget $ toCardId attrs)
            ]
            True
        | notNull validCards
        ]
  getActions iid window (ThePaintedWorld attrs) = getActions iid window attrs

instance HasModifiersFor env ThePaintedWorld

instance RunMessage env ThePaintedWorld where
  runMessage msg e@(ThePaintedWorld attrs) =
    ThePaintedWorld <$> runMessage msg attrs
