module Arkham.Location.Cards.CoterieLibrarySanctum (coterieLibrarySanctum) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Conspirator))

newtype CoterieLibrarySanctum = CoterieLibrarySanctum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coterieLibrarySanctum :: LocationCard CoterieLibrarySanctum
coterieLibrarySanctum = location CoterieLibrarySanctum Cards.coterieLibrarySanctum 4 (PerPlayer 1)

instance HasAbilities CoterieLibrarySanctum where
  getAbilities (CoterieLibrarySanctum a) =
    extendRevealed
      a
      [ restricted a 1 Here actionAbility
      , restricted a 2 Here $ FastAbility (ExhaustAssetCost $ AssetWithTrait Conspirator)
      ]

instance RunMessage CoterieLibrarySanctum where
  runMessage msg l@(CoterieLibrarySanctum attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      roundModifier (attrs.ability 1) iid (ReduceCostOf #asset 2)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      roundModifier (attrs.ability 1) iid (ReduceCostOf #asset 2)
      pure l
    _ -> CoterieLibrarySanctum <$> liftRunMessage msg attrs
