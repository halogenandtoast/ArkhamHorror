module Arkham.Enemy.Cards.LodgeJailor (lodgeJailor) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.I18n
import Arkham.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ForTheGreaterGood.Helpers
import Arkham.Trait (Trait (Sanctum))

newtype LodgeJailor = LodgeJailor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lodgeJailor :: EnemyCard LodgeJailor
lodgeJailor =
  enemy LodgeJailor Cards.lodgeJailor (2, Static 3, 3) (0, 2)
    & setSpawnAt (LocationWithTrait Sanctum)

instance HasAbilities LodgeJailor where
  getAbilities (LodgeJailor a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemySpawns #after Anywhere (be a)
      , skillTestAbility $ restricted a 2 OnSameLocation parleyAction_
      ]

instance RunMessage LodgeJailor where
  runMessage msg e@(LodgeJailor attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      mKey <- getRandomKey
      placeDoom (attrs.ability 1) attrs 2
      for_ mKey (placeKey attrs)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 2) attrs #intellect (Fixed 3)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      chooseOrRunOneM iid $ withI18n do
        when (attrs.token #doom > 0) do
          countVar 1 $ labeled' "removeDoom" $ removeDoom (attrs.ability 2) attrs 1
        for_ (setToList $ enemyKeys attrs) \k ->
          withVar "name" (String $ keyName k) $ labeled' "takeControlOfNamedKey" $ placeKey iid k
      pure e
    _ -> LodgeJailor <$> liftRunMessage msg attrs
