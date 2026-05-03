module Arkham.Location.Cards.PoisonedMarsh (poisonedMarsh) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype PoisonedMarsh = PoisonedMarsh LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

poisonedMarsh :: LocationCard PoisonedMarsh
poisonedMarsh = locationWith PoisonedMarsh Cards.poisonedMarsh 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities PoisonedMarsh where
  getAbilities (PoisonedMarsh a) =
    extendRevealed a [mkAbility a 1 $ forced (RevealLocation #after You $ be a)]

instance RunMessage PoisonedMarsh where
  runMessage msg l@(PoisonedMarsh attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ assetControlledBy iid
      chooseOrRunOneM iid do
        labeled "directDamage" $ directDamage iid (attrs.ability 1) 1
        unless (null assets) do
          labeled "assets" do
            for_ assets \asset -> dealAssetDamage asset (attrs.ability 1) 1
      pure l
    _ -> PoisonedMarsh <$> liftRunMessage msg attrs
