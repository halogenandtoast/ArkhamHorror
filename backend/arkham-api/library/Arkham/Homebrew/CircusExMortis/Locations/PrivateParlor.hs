module Arkham.Homebrew.CircusExMortis.Locations.PrivateParlor (privateParlor) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Ally, Creature))

newtype PrivateParlor = PrivateParlor LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

privateParlor :: LocationCard PrivateParlor
privateParlor = location PrivateParlor Cards.privateParlor 5 (PerPlayer 1)

instance HasModifiersFor PrivateParlor where
  getModifiersFor (PrivateParlor a) = viceShroudReduction a Intimacy

instance HasAbilities PrivateParlor where
  getAbilities (PrivateParlor a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ FastAbility
      $ ExhaustAssetCost
      $ AssetControlledBy You
      <> NotAsset (AssetWithTrait Creature <> AssetWithTrait Ally)

instance RunMessage PrivateParlor where
  runMessage msg l@(PrivateParlor attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      parleyBonusAt (attrs.ability 1) iid attrs.id
      pure l
    _ -> PrivateParlor <$> liftRunMessage msg attrs
