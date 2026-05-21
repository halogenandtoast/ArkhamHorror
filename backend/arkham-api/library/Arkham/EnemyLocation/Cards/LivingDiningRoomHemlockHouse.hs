module Arkham.EnemyLocation.Cards.LivingDiningRoomHemlockHouse (livingDiningRoomHemlockHouse) where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Story.Cards qualified as Stories

newtype LivingDiningRoomHemlockHouse = LivingDiningRoomHemlockHouse EnemyLocationAttrs
  deriving anyclass IsEnemyLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingDiningRoomHemlockHouse :: EnemyLocationCard LivingDiningRoomHemlockHouse
livingDiningRoomHemlockHouse =
  enemyLocationWith
    LivingDiningRoomHemlockHouse
    Cards.livingDiningRoomHemlockHouse
    (3, Static 4, 3)
    (1, 1)
    \la -> la {enemyLocationBase = (enemyLocationBase la) {locationShroud = Just (Static 2)}}

instance HasModifiersFor LivingDiningRoomHemlockHouse where
  getModifiersFor (LivingDiningRoomHemlockHouse a) = do
    n <- perPlayer 2
    modifySelf a [HealthModifier n]

instance HasAbilities LivingDiningRoomHemlockHouse where
  getAbilities (LivingDiningRoomHemlockHouse a) =
    extend1 a $ mkAbility a 1 $ forced $ FlipLocation #after Anyone (LocationWithId a.id)

instance RunMessage LivingDiningRoomHemlockHouse where
  runMessage msg el@(LivingDiningRoomHemlockHouse attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      thePredatoryHouse <- selectJust $ storyIs Stories.thePredatoryHouse
      sendMessage' thePredatoryHouse $ requestChaosTokens iid (attrs.ability 1) 1
      pure el
    _ -> LivingDiningRoomHemlockHouse <$> liftRunMessage msg attrs
