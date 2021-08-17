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
import Arkham.Types.Matcher hiding (DuringTurn)
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

newtype ThePaintedWorld = ThePaintedWorld EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePaintedWorld :: EventCard ThePaintedWorld
thePaintedWorld = event ThePaintedWorld Cards.thePaintedWorld

instance CanCheckPlayable env => RunMessage env ThePaintedWorld where
  runMessage msg e@(ThePaintedWorld attrs) = case msg of
    InvestigatorPlayEvent iid eid _ windows | eid == toId attrs -> do
      playableCards <-
        filterM
            (getIsPlayable iid (toSource attrs)
            $ Window Timing.When (DuringTurn iid)
            : windows
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
