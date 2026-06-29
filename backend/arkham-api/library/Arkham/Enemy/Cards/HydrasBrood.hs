module Arkham.Enemy.Cards.HydrasBrood (hydrasBrood) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.IntoTheMaelstrom.Helpers

newtype HydrasBrood = HydrasBrood EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hydrasBrood :: EnemyCard HydrasBrood
hydrasBrood =
  enemyWith HydrasBrood Cards.hydrasBrood
    $ spawnAtL
    ?~ SpawnAt
      ( FarthestLocationFromYou
          $ mapOneOf locationIs [Locations.lairOfHydra, Locations.gatewayToYhanthlei]
      )

instance HasAbilities HydrasBrood where
  getAbilities (HydrasBrood a) =
    extend1 a
      $ restricted
        a
        1
        (exists $ IncludeOmnipotent $ mapOneOf enemyIs [Cards.hydraDeepInSlumber, Cards.hydraAwakenedAndEnraged])
      $ forced
      $ EnemyEngaged #after You (be a)

instance RunMessage HydrasBrood where
  runMessage msg e@(HydrasBrood attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOne (IncludeOmnipotent $ enemyIs Cards.hydraDeepInSlumber) >>= \case
        Just hydra -> placeDoom (attrs.ability 1) hydra 1
        Nothing -> do
          hydra <- selectJust $ IncludeOmnipotent $ enemyIs Cards.hydraAwakenedAndEnraged
          chooseOneM iid $ scenarioI18n $ scope "hydrasBrood" do
            labeled' "placeDoomOnHydra" $ placeDoom (attrs.ability 1) hydra 1
            labeled' "hydraAttacksYou" $ initiateEnemyAttack hydra (attrs.ability 1) iid
      pure e
    _ -> HydrasBrood <$> liftRunMessage msg attrs
