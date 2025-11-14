module Arkham.Location.Cards.CountingRoom (countingRoom) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype CountingRoom = CountingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

countingRoom :: LocationCard CountingRoom
countingRoom = symbolLabel $ location CountingRoom Cards.countingRoom 3 (PerPlayer 1)

instance HasAbilities CountingRoom where
  getAbilities (CountingRoom a) =
    extendRevealed
      a
      [ restricted a 1 Here
          $ actionAbilityWithCost
          $ CostIfRemembered
            FoundAVent
            (GroupResourceCost (PerPlayer 5) (be a))
            (GroupResourceCost (PerPlayer 10) (be a))
      , playerLimit PerTurn
          $ restricted a 2 (Here <> can.gain.resources You)
          $ freeReaction
          $ SkillTestResult #after You (WhileInvestigating $ be a) (SuccessResult $ atLeast 2)
      ]

instance RunMessage CountingRoom where
  runMessage msg l@(CountingRoom attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      remember ObtainedASchematic
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      gainResources iid (attrs.ability 2) 2
      pure l
    _ -> CountingRoom <$> liftRunMessage msg attrs
