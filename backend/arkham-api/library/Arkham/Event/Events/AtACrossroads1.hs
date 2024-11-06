module Arkham.Event.Events.AtACrossroads1 (atACrossroads1, atACrossroads1Effect, AtACrossroads1 (..)) where

import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Matcher

newtype AtACrossroads1 = AtACrossroads1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

atACrossroads1 :: EventCard AtACrossroads1
atACrossroads1 = event AtACrossroads1 Cards.atACrossroads1

instance RunMessage AtACrossroads1 where
  runMessage msg e@(AtACrossroads1 attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      selectOneToHandle iid attrs $ affectsOthers Anyone
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (InvestigatorTarget iid') -> do
      chooseOneM iid do
        labeled
          "The chosen investigator must immediately take an action as if it were their turn, then discards 1 card at random from their hand."
          do
            takeActionAsIfTurn iid' attrs
            randomDiscard iid' attrs
        labeled "The chosen investigator loses 1 action during their next turn, then draws 3 cards." do
          createCardEffect Cards.atACrossroads1 Nothing attrs iid'
          drawCardsIfCan iid' attrs 3
      pure e
    _ -> AtACrossroads1 <$> liftRunMessage msg attrs

newtype AtACrossroads1Effect = AtACrossroads1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

atACrossroads1Effect :: EffectArgs -> AtACrossroads1Effect
atACrossroads1Effect = cardEffect AtACrossroads1Effect Cards.atACrossroads1

instance RunMessage AtACrossroads1Effect where
  runMessage msg e@(AtACrossroads1Effect attrs) = runQueueT $ case msg of
    BeginTurn iid | isTarget iid attrs.target -> do
      push $ LoseActions iid attrs.source 1
      disableReturn e
    _ -> AtACrossroads1Effect <$> liftRunMessage msg attrs
