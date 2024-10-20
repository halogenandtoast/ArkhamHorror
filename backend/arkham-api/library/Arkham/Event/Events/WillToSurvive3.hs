module Arkham.Event.Events.WillToSurvive3 where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Prelude

newtype WillToSurvive3 = WillToSurvive3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

willToSurvive3 :: EventCard WillToSurvive3
willToSurvive3 = event WillToSurvive3 Cards.willToSurvive3

instance RunMessage WillToSurvive3 where
  runMessage msg e@(WillToSurvive3 attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      pushM $ turnModifier iid attrs iid DoNotDrawChaosTokensForSkillChecks
      pure e
    _ -> WillToSurvive3 <$> runMessage msg attrs
