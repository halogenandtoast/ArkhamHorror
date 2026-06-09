module Arkham.EnemyLocation.Cards.LivingBedroomHemlockHouse34 (livingBedroomHemlockHouse34) where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Story.Cards qualified as Stories

newtype LivingBedroomHemlockHouse34 = LivingBedroomHemlockHouse34 EnemyLocationAttrs
  deriving anyclass IsEnemyLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingBedroomHemlockHouse34 :: EnemyLocationCard LivingBedroomHemlockHouse34
livingBedroomHemlockHouse34 =
  enemyLocationWith
    LivingBedroomHemlockHouse34
    Cards.livingBedroomHemlockHouse34
    (3, Static 3, 2)
    (1, 1)
    \la -> la {enemyLocationBase = (enemyLocationBase la) {locationShroud = Just (Static 3)}}

instance HasModifiersFor LivingBedroomHemlockHouse34 where
  getModifiersFor (LivingBedroomHemlockHouse34 a) = do
    n <- perPlayer 2
    modifySelf a [HealthModifier n]

instance HasAbilities LivingBedroomHemlockHouse34 where
  getAbilities (LivingBedroomHemlockHouse34 a) =
    extend1 a $ mkAbility a 1 $ forced $ FlipLocation #after Anyone (LocationWithId a.id)

instance RunMessage LivingBedroomHemlockHouse34 where
  runMessage msg el@(LivingBedroomHemlockHouse34 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- If The Predatory House is not in play yet (agenda 1b flips before it
      -- resolves), there is no predation bag and the test fizzles.
      selectOne (storyIs Stories.thePredatoryHouse) >>= traverse_ \thePredatoryHouse ->
        sendMessage' thePredatoryHouse $ requestChaosTokens iid (attrs.ability 1) 1
      pure el
    _ -> LivingBedroomHemlockHouse34 <$> liftRunMessage msg attrs
