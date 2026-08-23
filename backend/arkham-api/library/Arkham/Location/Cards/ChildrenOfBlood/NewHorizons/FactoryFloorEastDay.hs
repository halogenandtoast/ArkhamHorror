module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.FactoryFloorEastDay (factoryFloorEastDay) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FactoryFloorEastDay = FactoryFloorEastDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

factoryFloorEastDay :: LocationCard FactoryFloorEastDay
factoryFloorEastDay = symbolLabel $ location FactoryFloorEastDay Cards.factoryFloorEastDay 2 (PerPlayer 1)

instance HasAbilities FactoryFloorEastDay where
  getAbilities (FactoryFloorEastDay a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoverClues #after You (be a) (atLeast 1)

instance RunMessage FactoryFloorEastDay where
  runMessage msg l@(FactoryFloorEastDay attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (investigatorAt attrs) \iid -> chooseAndDiscardCard iid (attrs.ability 1)
      pure l
    _ -> FactoryFloorEastDay <$> liftRunMessage msg attrs
