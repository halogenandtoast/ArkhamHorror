module Arkham.EnemyLocation.Cards.LivingDiningRoomHemlockHouse (livingDiningRoomHemlockHouse) where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Matcher
import Arkham.Story.Cards qualified as Stories

newtype LivingDiningRoomHemlockHouse = LivingDiningRoomHemlockHouse EnemyLocationAttrs
  deriving anyclass (IsEnemyLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Living Dining Room: Massive. Cannot make attacks of opportunity.
-- Living Dining Room gets +2 [per_investigator] health.
livingDiningRoomHemlockHouse :: EnemyLocationCard LivingDiningRoomHemlockHouse
livingDiningRoomHemlockHouse =
  enemyLocationWith
    LivingDiningRoomHemlockHouse
    Cards.livingDiningRoomHemlockHouse
    (2, StaticWithPerPlayer 4 2, 3)
    (1, 1)
    (\la -> la {enemyLocationBase = (enemyLocationBase la) {locationShroud = Just (Static 2)}})

instance HasAbilities LivingDiningRoomHemlockHouse where
  getAbilities (LivingDiningRoomHemlockHouse a) =
    getAbilities a
      <> [ -- "Forced - When this enemy-location is revealed: Make a predation test."
           mkAbility a 1
             $ forced
             $ FlipLocation #after Anyone (LocationWithId a.id)
         ]

instance RunMessage LivingDiningRoomHemlockHouse where
  runMessage msg el@(LivingDiningRoomHemlockHouse attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      thePredatoryHouse <- selectJust $ storyIs Stories.thePredatoryHouse
      sendMessage' thePredatoryHouse $ requestChaosTokens iid (attrs.ability 1) 1
      pure el
    _ -> LivingDiningRoomHemlockHouse <$> liftRunMessage msg attrs
