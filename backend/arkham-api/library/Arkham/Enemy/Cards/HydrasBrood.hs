module Arkham.Enemy.Cards.HydrasBrood (hydrasBrood, HydrasBrood (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype HydrasBrood = HydrasBrood EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hydrasBrood :: EnemyCard HydrasBrood
hydrasBrood =
  enemyWith HydrasBrood Cards.hydrasBrood (1, Static 4, 3) (1, 0)
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
        ( exists
            $ mapOneOf
              enemyIs
              [Cards.hydraDeepInSlumber, Cards.hydraAwakenedAndEnraged]
        )
      $ forced
      $ EnemyEngaged #after You (be a)

instance RunMessage HydrasBrood where
  runMessage msg e@(HydrasBrood attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mHydraSlumbering <- selectOne $ enemyIs Cards.hydraDeepInSlumber
      case mHydraSlumbering of
        Just hydra -> placeDoom (attrs.ability 1) hydra 1
        Nothing -> do
          hydra <- selectJust $ enemyIs Cards.hydraAwakenedAndEnraged
          chooseOneM iid do
            labeled "Place 1 doom on Hydra" $ placeDoom (attrs.ability 1) hydra 1
            labeled "Hydra attacks you" $ initiateEnemyAttack hydra (attrs.ability 1) iid
      pure e
    _ -> HydrasBrood <$> liftRunMessage msg attrs
