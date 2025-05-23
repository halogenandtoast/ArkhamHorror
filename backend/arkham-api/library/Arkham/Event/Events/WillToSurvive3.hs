module Arkham.Event.Events.WillToSurvive3 (willToSurvive3) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Modifier

newtype WillToSurvive3 = WillToSurvive3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

willToSurvive3 :: EventCard WillToSurvive3
willToSurvive3 = event WillToSurvive3 Cards.willToSurvive3

instance RunMessage WillToSurvive3 where
  runMessage msg e@(WillToSurvive3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      turnModifier iid attrs iid DoNotDrawChaosTokensForSkillChecks
      pure e
    _ -> WillToSurvive3 <$> liftRunMessage msg attrs
