module Arkham.Event.Cards.BideYourTime (bideYourTime, bideYourTimeEffect, BideYourTime (..)) where

import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted

newtype BideYourTime = BideYourTime EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bideYourTime :: EventCard BideYourTime
bideYourTime = event BideYourTime Cards.bideYourTime

instance RunMessage BideYourTime where
  runMessage msg e@(BideYourTime attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      createCardEffect Cards.bideYourTime Nothing attrs iid
      pure e
    _ -> BideYourTime <$> liftRunMessage msg attrs

newtype BideYourTimeEffect = BideYourTimeEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bideYourTimeEffect :: EffectArgs -> BideYourTimeEffect
bideYourTimeEffect = cardEffect BideYourTimeEffect Cards.bideYourTime

instance RunMessage BideYourTimeEffect where
  runMessage msg e@(BideYourTimeEffect attrs) = runQueueT $ case msg of
    BeginTurn iid | isTarget iid attrs.target -> do
      push $ GainActions iid attrs.source 2
      disableReturn e
    _ -> BideYourTimeEffect <$> liftRunMessage msg attrs
