module Arkham.Asset.Assets.DimensionalBeamMachine (dimensionalBeamMachine) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.MachinationsThroughTime.Helpers

newtype DimensionalBeamMachine = DimensionalBeamMachine AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalBeamMachine :: AssetCard DimensionalBeamMachine
dimensionalBeamMachine = asset DimensionalBeamMachine Cards.dimensionalBeamMachine

instance HasAbilities DimensionalBeamMachine where
  getAbilities (DimensionalBeamMachine a) =
    [ controlled a 1 (DuringTurn You)
        $ FastAbility (exhaust a <> assetUseCost a Charge 1)
    ]

instance RunMessage DimensionalBeamMachine where
  runMessage msg a@(DimensionalBeamMachine attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ colocatedWith iid
      enemies <- select $ NonEliteEnemy <> enemyAtLocationWith iid
      destinations <- select RevealedLocation
      chooseOneM iid $ scenarioI18n do
        labeled' "dimensionalBeamMachine.investigator" do
          chooseTargetM iid investigators \investigator ->
            chooseTargetM iid destinations \destination ->
              moveTo (attrs.ability 1) investigator destination
        when (notNull enemies) do
          labeled' "dimensionalBeamMachine.enemy" do
            chooseTargetM iid enemies \enemy ->
              chooseTargetM iid destinations \destination ->
                push $ EnemyMove enemy destination
      pure a
    _ -> DimensionalBeamMachine <$> liftRunMessage msg attrs
