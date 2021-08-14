module Arkham.Types.Event.Cards.ThePaintedWorld
  ( thePaintedWorld
  , ThePaintedWorld(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Card.CardMatcher
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Game.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window

newtype ThePaintedWorld = ThePaintedWorld EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePaintedWorld :: EventCard ThePaintedWorld
thePaintedWorld = event ThePaintedWorld Cards.thePaintedWorld

instance HasAbilities env ThePaintedWorld where
  getAbilities iid window (ThePaintedWorld attrs) = getAbilities iid window attrs

instance HasModifiersFor env ThePaintedWorld

instance CanCheckPlayable env => RunMessage env ThePaintedWorld where
  runMessage msg e@(ThePaintedWorld attrs) = case msg of
    InvestigatorPlayEvent iid eid _ windows | eid == toId attrs -> do
      playableCards <-
        filterM (getIsPlayable iid $ DuringTurn iid : windows) =<< getList
          (BasicCardMatch (NonExceptional <> EventCard)
          <> CardIsBeneathInvestigator You
          )
      e <$ push
        (InitiatePlayCardAsChoose
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
        )
    _ -> ThePaintedWorld <$> runMessage msg attrs
