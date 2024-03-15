module Arkham.Event.Cards.EideticMemory3 (eideticMemory3, eideticMemory3Effect, EideticMemory3 (..)) where

import Arkham.Capability
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Effect.Runner
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Game.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Trait
import Arkham.Window

newtype EideticMemory3 = EideticMemory3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eideticMemory3 :: EventCard EideticMemory3
eideticMemory3 =
  eventWith EideticMemory3 Cards.eideticMemory3 $ afterPlayL .~ RemoveThisFromGame

instance RunMessage EideticMemory3 where
  runMessage msg e@(EideticMemory3 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ windows' _ | eid == toId attrs -> do
      discards <- map PlayerCard <$> selectAgg id InvestigatorDiscard UneliminatedInvestigator
      let candidates = filter (`cardMatch` (CardWithTrait Insight <> Matcher.EventCard)) discards
      let playableWindows = if null windows' then [mkWhen (DuringTurn iid)] else windows'
      playableCards <- filterM (getIsPlayable iid (toSource attrs) UnpaidCost playableWindows) candidates
      canAffectOthers <- can.affect.otherPlayers iid
      push
        $ InitiatePlayCardAsChoose
          iid
          (toCard attrs)
          playableCards
          [ createCardEffect
              Cards.eideticMemory3
              Nothing
              (CardSource $ toCard attrs)
              (CardIdTarget $ toCardId attrs)
          ]
          (if canAffectOthers then RemoveChosenCardFromGame else LeaveChosenCard)
          NoPayment
          playableWindows
          True
      pure e
    _ -> EideticMemory3 <$> runMessage msg attrs

newtype EideticMemory3Effect = EideticMemory3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eideticMemory3Effect :: EffectArgs -> EideticMemory3Effect
eideticMemory3Effect = cardEffect EideticMemory3Effect Cards.eideticMemory3

instance HasModifiersFor EideticMemory3Effect where
  getModifiersFor (EventTarget eid) (EideticMemory3Effect a) = do
    cardId <- field EventCardId eid
    pure $ toModifiers a [RemoveFromGameInsteadOfDiscard | toTarget cardId == a.target]
  getModifiersFor (CardIdTarget cardId) (EideticMemory3Effect a) = do
    pure $ toModifiers a [RemoveFromGameInsteadOfDiscard | toTarget cardId == a.target]
  getModifiersFor _ _ = pure []

instance RunMessage EideticMemory3Effect where
  runMessage msg (EideticMemory3Effect attrs) = EideticMemory3Effect <$> runMessage msg attrs
