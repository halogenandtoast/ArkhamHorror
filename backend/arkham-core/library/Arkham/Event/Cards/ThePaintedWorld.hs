module Arkham.Event.Cards.ThePaintedWorld
  ( thePaintedWorld
  , ThePaintedWorld(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Game.Helpers
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window

newtype ThePaintedWorld = ThePaintedWorld EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePaintedWorld :: EventCard ThePaintedWorld
thePaintedWorld = event ThePaintedWorld Cards.thePaintedWorld

instance CanCheckPlayable env => RunMessage env ThePaintedWorld where
  runMessage msg e@(ThePaintedWorld attrs) = case msg of
    InvestigatorPlayEvent iid eid _ windows' _ | eid == toId attrs -> do
      playableCards <-
        filterM
            (getIsPlayable iid (toSource attrs) UnpaidCost
            $ Window Timing.When (DuringTurn iid)
            : windows'
            )
          =<< getList
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
