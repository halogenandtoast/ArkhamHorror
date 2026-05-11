module Arkham.EnemyLocation.Cards.LivingWashroomHemlockHouse38 (livingWashroomHemlockHouse38) where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted (randomDiscardN)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher

newtype LivingWashroomHemlockHouse38 = LivingWashroomHemlockHouse38 EnemyLocationAttrs
  deriving anyclass (IsEnemyLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingWashroomHemlockHouse38 :: EnemyLocationCard LivingWashroomHemlockHouse38
livingWashroomHemlockHouse38 =
  enemyLocationWith
    LivingWashroomHemlockHouse38
    Cards.livingWashroomHemlockHouse38
    (2, Static 3, 3)
    (1, 1)
    (\la -> la {enemyLocationBase = (enemyLocationBase la) {locationShroud = Just (Static 4)}})

instance HasAbilities LivingWashroomHemlockHouse38 where
  getAbilities (LivingWashroomHemlockHouse38 a) =
    getAbilities a
      <> [ -- "Forced - When this enemy-location is revealed: Each investigator
           -- at this location discards cards at random from their hand equal to
           -- the number of clues on this location."
           mkAbility a 1
             $ forced
             $ FlipLocation #after Anyone (LocationWithId a.id)
         ]

instance RunMessage LivingWashroomHemlockHouse38 where
  runMessage msg el@(LivingWashroomHemlockHouse38 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      let n = attrs.clues
      when (n > 0) $ do
        iids <- allInvestigators
        for_ iids $ \iid ->
          withLocationOf iid $ \lid ->
            when (lid == attrs.id) (randomDiscardN iid (attrs.ability 1) n)
      pure el
    _ -> LivingWashroomHemlockHouse38 <$> liftRunMessage msg attrs
