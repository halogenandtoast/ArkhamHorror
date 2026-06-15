module Arkham.EnemyLocation.Cards.LivingBedroomHemlockHouse32 (livingBedroomHemlockHouse32) where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Token

newtype LivingBedroomHemlockHouse32 = LivingBedroomHemlockHouse32 EnemyLocationAttrs
  deriving anyclass IsEnemyLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingBedroomHemlockHouse32 :: EnemyLocationCard LivingBedroomHemlockHouse32
livingBedroomHemlockHouse32 =
  enemyLocationWith
    LivingBedroomHemlockHouse32
    Cards.livingBedroomHemlockHouse32
    (3, Static 3, 2)
    (2, 0)
    \la -> la {enemyLocationBase = (enemyLocationBase la) {locationShroud = Just (Static 3)}}

instance HasModifiersFor LivingBedroomHemlockHouse32 where
  getModifiersFor (LivingBedroomHemlockHouse32 a) = do
    n <- perPlayer 2
    modifySelf a [HealthModifier n]

instance HasAbilities LivingBedroomHemlockHouse32 where
  getAbilities (LivingBedroomHemlockHouse32 a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ FlipLocation #after Anyone (LocationWithId a.id <> LocationWithInvestigator Anyone)

instance RunMessage LivingBedroomHemlockHouse32 where
  runMessage msg el@(LivingBedroomHemlockHouse32 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs Doom 1
      pure el
    _ -> LivingBedroomHemlockHouse32 <$> liftRunMessage msg attrs
