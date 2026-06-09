module Arkham.EnemyLocation.Cards.LivingWashroomHemlockHouse38 (livingWashroomHemlockHouse38) where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted (randomDiscardN)
import Arkham.Matcher

newtype LivingWashroomHemlockHouse38 = LivingWashroomHemlockHouse38 EnemyLocationAttrs
  deriving anyclass (IsEnemyLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingWashroomHemlockHouse38 :: EnemyLocationCard LivingWashroomHemlockHouse38
livingWashroomHemlockHouse38 =
  enemyLocationWith
    LivingWashroomHemlockHouse38
    Cards.livingWashroomHemlockHouse38
    (3, PerPlayer 3, 3)
    (1, 1)
    \la -> la {enemyLocationBase = (enemyLocationBase la) {locationShroud = Just (Static 4)}}

instance HasAbilities LivingWashroomHemlockHouse38 where
  getAbilities (LivingWashroomHemlockHouse38 a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ FlipLocation #after Anyone
      $ LocationWithId a.id
      <> LocationWithAnyClues
      <> LocationWithInvestigator (HandWith AnyCards)

instance RunMessage LivingWashroomHemlockHouse38 where
  runMessage msg el@(LivingWashroomHemlockHouse38 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      let n = attrs.clues
      when (n > 0) do
        selectEach (InvestigatorAt (LocationWithId attrs.id) <> HandWith AnyCards) \iid ->
          randomDiscardN iid (attrs.ability 1) n
      pure el
    _ -> LivingWashroomHemlockHouse38 <$> liftRunMessage msg attrs
