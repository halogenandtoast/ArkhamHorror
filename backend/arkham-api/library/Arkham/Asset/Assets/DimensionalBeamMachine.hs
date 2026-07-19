module Arkham.Asset.Assets.DimensionalBeamMachine (dimensionalBeamMachine) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (InvestigatorEliminated)
import Arkham.Asset.Uses
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
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
      <> [ mkAbility a 2
             $ SilentForcedAbility
             $ InvestigatorEliminated #when (InvestigatorWithId controller)
         | controller <- maybeToList a.controller
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
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      abduct attrs
      pure a
    _ -> DimensionalBeamMachine <$> liftRunMessage msg attrs
