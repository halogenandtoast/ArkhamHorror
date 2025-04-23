module Arkham.Enemy.Cards.DevoteeOfTheKey (devoteeOfTheKey) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype DevoteeOfTheKey = DevoteeOfTheKey EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devoteeOfTheKey :: EnemyCard DevoteeOfTheKey
devoteeOfTheKey =
  enemyWith DevoteeOfTheKey Cards.devoteeOfTheKey (3, Static 3, 3) (1, 1)
    $ spawnAtL
    ?~ "Base of the Hill"

instance HasAbilities DevoteeOfTheKey where
  getAbilities (DevoteeOfTheKey a) =
    extend1 a
      $ restricted a 1 (exists $ ClosestPathLocationMatch (locationWithEnemy a) "Sentinel Peak")
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage DevoteeOfTheKey where
  runMessage msg e@(DevoteeOfTheKey attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      getLocationOf attrs >>= traverse_ \loc -> do
        sentinelPeak <- selectJust $ LocationWithTitle "Sentinel Peak"
        if loc == sentinelPeak
          then do
            toDiscard (attrs.ability 1) attrs
            placeDoomOnAgenda 2
          else do
            choices <- select $ ClosestPathLocation loc sentinelPeak
            lead <- getLead
            case choices of
              [] -> error "should not happen"
              xs -> chooseOrRunOneM lead $ targets xs (enemyMoveTo attrs)
      pure e
    _ -> DevoteeOfTheKey <$> liftRunMessage msg attrs
