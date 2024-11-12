module Arkham.Enemy.Cards.DagonsBrood (dagonsBrood, DagonsBrood (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype DagonsBrood = DagonsBrood EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dagonsBrood :: EnemyCard DagonsBrood
dagonsBrood =
  enemyWith DagonsBrood Cards.dagonsBrood (1, Static 4, 3) (1, 0)
    $ spawnAtL
    ?~ SpawnAt
      ( FarthestLocationFromYou
          $ mapOneOf locationIs [Locations.lairOfDagonIntoTheMaelstrom, Locations.gatewayToYhanthlei]
      )

instance HasAbilities DagonsBrood where
  getAbilities (DagonsBrood a) =
    extend1 a
      $ restricted
        a
        1
        ( exists
            $ mapOneOf
              enemyIs
              [Cards.dagonDeepInSlumberIntoTheMaelstrom, Cards.dagonAwakenedAndEnragedIntoTheMaelstrom]
        )
      $ forced
      $ EnemyEngaged #after You (be a)

instance RunMessage DagonsBrood where
  runMessage msg e@(DagonsBrood attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mDagonSlumbering <- selectOne $ enemyIs Cards.dagonDeepInSlumberIntoTheMaelstrom
      case mDagonSlumbering of
        Just dagon -> placeDoom (attrs.ability 1) dagon 1
        Nothing -> do
          dagon <- selectJust $ enemyIs Cards.dagonAwakenedAndEnragedIntoTheMaelstrom
          chooseOneM iid do
            labeled "Place 1 doom on Dagon" $ placeDoom (attrs.ability 1) dagon 1
            labeled "Dagon attacks you" $ initiateEnemyAttack dagon (attrs.ability 1) iid
      pure e
    _ -> DagonsBrood <$> liftRunMessage msg attrs
