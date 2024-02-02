module Arkham.Event.Cards.EideticMemory3 (
  eideticMemory3,
  EideticMemory3 (..),
)
where

import Arkham.Prelude

import Arkham.Capability
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Game.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Matcher qualified as Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window

newtype EideticMemory3 = EideticMemory3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

eideticMemory3 :: EventCard EideticMemory3
eideticMemory3 =
  eventWith EideticMemory3 Cards.eideticMemory3 $ afterPlayL .~ RemoveThisFromGame

instance RunMessage EideticMemory3 where
  runMessage msg e@(EideticMemory3 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ windows' _ | eid == toId attrs -> do
      discards <- map PlayerCard <$> selectAgg id InvestigatorDiscard UneliminatedInvestigator
      let
        candidates =
          filter (`cardMatch` (CardWithTrait Insight <> Matcher.EventCard)) discards
        playableWindows =
          if null windows'
            then [mkWindow Timing.When (DuringTurn iid)]
            else windows'
      playableCards <-
        filterM
          (getIsPlayable iid (toSource attrs) UnpaidCost playableWindows)
          candidates
      canAffectOthers <- can.affect.otherPlayers iid
      push
        $ InitiatePlayCardAsChoose
          iid
          (toCard attrs)
          playableCards
          [ CreateEffect
              "03306"
              Nothing
              (CardSource $ toCard attrs)
              (CardIdTarget $ toCardId attrs)
          ]
          (if canAffectOthers then RemoveChosenCardFromGame else LeaveChosenCard)
          playableWindows
          True
      pure e
    _ -> EideticMemory3 <$> runMessage msg attrs
