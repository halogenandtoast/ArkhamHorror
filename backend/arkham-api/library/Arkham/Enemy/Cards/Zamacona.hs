module Arkham.Enemy.Cards.Zamacona (zamacona, Zamacona (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher

newtype Meta = Meta {punishParley :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Zamacona = Zamacona (EnemyAttrs `With` Meta)
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor Zamacona where
  getModifiersFor (Zamacona (With attrs _)) =
    modifySelect
      attrs
      (investigatorIs Investigators.alessandraZorzi)
      [CannotParleyWith (EnemyWithId attrs.id)]

instance HasAbilities Zamacona where
  getAbilities (Zamacona (attrs `With` meta)) =
    extend
      attrs
      [ restrictedAbility attrs 1 (youExist (investigatorIs Investigators.alessandraZorzi) <> criteria)
          $ forced
          $ FirstTimeParleyingThisRound #when (investigatorIs Investigators.alessandraZorzi)
      ]
   where
    criteria = if punishParley meta then NoRestriction else Never

zamacona :: EnemyCard Zamacona
zamacona =
  enemyWith (Zamacona . (`with` Meta True)) Cards.zamacona (3, Static 3, 3) (1, 0)
    $ spawnAtL
    ?~ SpawnAtFirst [SpawnAt (NearestLocationToYou EmptyLocation), SpawnAt YourLocation]

instance RunMessage Zamacona where
  runMessage msg (Zamacona (With attrs meta)) = runQueueT $ case msg of
    BeginRound -> pure . Zamacona $ attrs `with` Meta True
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure . Zamacona $ attrs `with` Meta False
    _ -> Zamacona . (`with` meta) <$> liftRunMessage msg attrs
