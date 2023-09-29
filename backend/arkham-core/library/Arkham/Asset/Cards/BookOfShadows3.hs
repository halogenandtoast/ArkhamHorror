module Arkham.Asset.Cards.BookOfShadows3 (
  BookOfShadows3 (..),
  bookOfShadows3,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.Trait

newtype BookOfShadows3 = BookOfShadows3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bookOfShadows3 :: AssetCard BookOfShadows3
bookOfShadows3 = asset BookOfShadows3 Cards.bookOfShadows3

slot :: AssetAttrs -> Slot
slot attrs = Slot (toSource attrs) []

instance HasAbilities BookOfShadows3 where
  getAbilities (BookOfShadows3 a) =
    [ controlledAbility a 1 (exists $ AssetControlledBy You <> withTrait Spell)
        $ actionAbilityWithCost (exhaust a)
    ]

instance RunMessage BookOfShadows3 where
  runMessage msg a@(BookOfShadows3 attrs) = case msg of
    -- Slots need to be added before the asset is played so we hook into played card
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      push $ AddSlot iid #arcane (slot attrs)
      BookOfShadows3 <$> runMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      spellAssets <- selectList $ assetControlledBy iid <> withTrait Spell
      pushIfAny spellAssets
        $ chooseOne iid
        $ targetLabels spellAssets (\spellAsset -> only $ AddUses spellAsset Charge 1)
      pure a
    _ -> BookOfShadows3 <$> runMessage msg attrs
