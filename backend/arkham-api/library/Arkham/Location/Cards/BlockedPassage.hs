module Arkham.Location.Cards.BlockedPassage (blockedPassage, BlockedPassage (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype BlockedPassage = BlockedPassage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blockedPassage :: LocationCard BlockedPassage
blockedPassage =
  locationWith BlockedPassage Cards.blockedPassage 7 (Static 0)
    $ (connectsToL .~ adjacentLocations)
    . (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasAbilities BlockedPassage where
  getAbilities (BlockedPassage a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #when You (be a)

instance RunMessage BlockedPassage where
  runMessage msg l@(BlockedPassage attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 2
      roundModifier (attrs.ability 1) iid CannotMove
      pure l
    _ -> BlockedPassage <$> liftRunMessage msg attrs
