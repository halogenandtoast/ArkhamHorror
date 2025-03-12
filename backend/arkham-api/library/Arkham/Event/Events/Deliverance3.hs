module Arkham.Event.Events.Deliverance3 (deliverance3, deliverance3Effect) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Effect.Import
import Arkham.Modifier
import Arkham.Phase
import Arkham.Keyword qualified as Keyword

newtype Deliverance3 = Deliverance3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deliverance3 :: EventCard Deliverance3
deliverance3 = event Deliverance3 Cards.deliverance3

instance RunMessage Deliverance3 where
  runMessage msg e@(Deliverance3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      phaseModifier attrs (CardMatcherTarget #any) EffectsCannotBeCanceled
      createCardEffect Cards.deliverance3 Nothing attrs iid
      phaseModifier attrs iid DrawsEachEncounterCard
      endOfNextPhaseModifier
        MythosPhase
        attrs
        (PhaseTarget MythosPhase)
        (SkipMythosPhaseStep EachInvestigatorDrawsEncounterCardStep)
      pure e
    _ -> Deliverance3 <$> liftRunMessage msg attrs

newtype Deliverance3Effect = Deliverance3Effect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deliverance3Effect :: EffectArgs -> Deliverance3Effect
deliverance3Effect = cardEffect Deliverance3Effect Cards.deliverance3

instance RunMessage Deliverance3Effect where
  runMessage msg e@(Deliverance3Effect attrs) = runQueueT $ case msg of
    DrewCards iid drew | isTarget iid attrs.target -> do
      case headMay drew.cards of
        Just card -> do
          cardResolutionModifier card attrs.source card (AddKeyword Keyword.Surge)
          disableReturn e
        Nothing -> pure e
    EndPhase -> disableReturn e
    _ -> Deliverance3Effect <$> liftRunMessage msg attrs
