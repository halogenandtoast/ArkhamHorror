module Arkham.Location.Cards.KnightsHall (knightsHall) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.GameValue
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Investigate
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Scenarios.BlackStarsRise.Helpers

newtype KnightsHall = KnightsHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knightsHall :: LocationCard KnightsHall
knightsHall = location KnightsHall Cards.knightsHall 2 (PerPlayer 1)

instance HasAbilities KnightsHall where
  getAbilities (KnightsHall a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "knightsHall.investigate"
      $ restricted a 1 (Here <> NoCluesOnThis) investigateAction_

instance RunMessage KnightsHall where
  runMessage msg l@(KnightsHall attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigateEdit_ sid iid (attrs.ability 1) (withSkillType #agility)
      pure l
    Successful (Action.Investigate, _) _ (isAbilitySource attrs 1 -> True) _ _ -> do
      remember FoundTheTowerKey
      pure l
    _ -> KnightsHall <$> liftRunMessage msg attrs
