module Arkham.EnemyLocation.Cards.LivingBedroomHemlockHouse34 (livingBedroomHemlockHouse34) where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Matcher
import Arkham.Story.Cards qualified as Stories

newtype LivingBedroomHemlockHouse34 = LivingBedroomHemlockHouse34 EnemyLocationAttrs
  deriving anyclass (IsEnemyLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingBedroomHemlockHouse34 :: EnemyLocationCard LivingBedroomHemlockHouse34
livingBedroomHemlockHouse34 =
  enemyLocationWith
    LivingBedroomHemlockHouse34
    Cards.livingBedroomHemlockHouse34
    (2, StaticWithPerPlayer 3 2, 3)
    (1, 1)
    (\la -> la {enemyLocationBase = (enemyLocationBase la) {locationShroud = Just (Static 3)}})

instance HasAbilities LivingBedroomHemlockHouse34 where
  getAbilities (LivingBedroomHemlockHouse34 a) =
    getAbilities a
      <> [ -- "Forced - When this enemy-location is revealed: Make a predation test."
           mkAbility a 1
             $ forced
             $ FlipLocation #after Anyone (LocationWithId a.id)
         ]

instance RunMessage LivingBedroomHemlockHouse34 where
  runMessage msg el@(LivingBedroomHemlockHouse34 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      thePredatoryHouse <- selectJust $ storyIs Stories.thePredatoryHouse
      sendMessage' thePredatoryHouse $ requestChaosTokens iid (attrs.ability 1) 1
      pure el
    _ -> LivingBedroomHemlockHouse34 <$> liftRunMessage msg attrs
