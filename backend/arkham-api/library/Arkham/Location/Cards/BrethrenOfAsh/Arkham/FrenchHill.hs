{- HLINT ignore "Use camelCase" -}
module Arkham.Location.Cards.BrethrenOfAsh.Arkham.FrenchHill (frenchHill) where

import Arkham.Ability
import Arkham.Asset.Uses
import Arkham.GameValue
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Location.CardDefs.BrethrenOfAsh.Arkham qualified as Cards (frenchHill)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype FrenchHill = FrenchHill LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frenchHill :: LocationCard FrenchHill
frenchHill = location FrenchHill Cards.frenchHill 4 (PerPlayer 2)

instance HasAbilities FrenchHill where
  getAbilities (FrenchHill a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted
        a
        1
        (Here <> exists (AssetControlledBy You <> mapOneOf AssetCanHaveUses [Charge, Secret]))
        actionAbility

instance RunMessage FrenchHill where
  runMessage msg l@(FrenchHill attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAndDiscardCards iid attrs 1
      assets <- select $ assetControlledBy iid <> mapOneOf AssetCanHaveUses [Charge, Secret]
      chooseTargetM iid assets \asset -> do
        canHaveCharge <- asset <=~> AssetCanHaveUses Charge
        canHaveSecret <- asset <=~> AssetCanHaveUses Secret
        if
          | canHaveCharge && canHaveSecret ->
              chooseOneM iid $ withI18n do
                tokenVar Charge $ labeled "placeToken" $ addUses (attrs.ability 1) asset Charge 1
                tokenVar Secret $ labeled "placeToken" $ addUses (attrs.ability 1) asset Secret 1
          | canHaveCharge -> addUses (attrs.ability 1) asset Charge 1
          | canHaveSecret -> addUses (attrs.ability 1) asset Secret 1
          | otherwise -> pure ()
      pure l
    _ -> FrenchHill <$> liftRunMessage msg attrs
