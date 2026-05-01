module Arkham.Location.Cards.FrenchHill_c2026 (frenchHill_c2026) where

import Arkham.Ability
import Arkham.Asset.Uses
import Arkham.GameValue
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards (frenchHill_c2026)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype FrenchHill_c2026 = FrenchHill_c2026 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frenchHill_c2026 :: LocationCard FrenchHill_c2026
frenchHill_c2026 = location FrenchHill_c2026 Cards.frenchHill_c2026 4 (PerPlayer 2)

instance HasAbilities FrenchHill_c2026 where
  getAbilities (FrenchHill_c2026 a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted
        a
        1
        (Here <> exists (AssetControlledBy You <> mapOneOf AssetCanHaveUses [Charge, Secret]))
        actionAbility

instance RunMessage FrenchHill_c2026 where
  runMessage msg l@(FrenchHill_c2026 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAndDiscardCards iid attrs 1
      assets <- select $ assetControlledBy iid <> mapOneOf AssetCanHaveUses [Charge, Secret]
      chooseTargetM iid assets \asset -> do
        canHaveCharge <- asset <=~> AssetCanHaveUses Charge
        canHaveSecret <- asset <=~> AssetCanHaveUses Secret
        if
          | canHaveCharge && canHaveSecret ->
              chooseOneM iid $ withI18n do
                tokenVar Charge $ labeled' "placeToken" $ addUses (attrs.ability 1) asset Charge 1
                tokenVar Secret $ labeled' "placeToken" $ addUses (attrs.ability 1) asset Secret 1
          | canHaveCharge -> addUses (attrs.ability 1) asset Charge 1
          | canHaveSecret -> addUses (attrs.ability 1) asset Secret 1
          | otherwise -> pure ()
      pure l
    _ -> FrenchHill_c2026 <$> liftRunMessage msg attrs
