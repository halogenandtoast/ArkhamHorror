module Arkham.Asset.Assets.EnchantedSkull (enchantedSkull) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card (setFacedown, toCard)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Modifier

newtype EnchantedSkull = EnchantedSkull AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantedSkull :: AssetCard EnchantedSkull
enchantedSkull = asset EnchantedSkull Cards.enchantedSkull

instance HasAbilities EnchantedSkull where
  getAbilities (EnchantedSkull a) =
    [ controlled_ a 1 $ FastAbility $ exhaust a
    , playerLimit PerTurn
        $ controlled a 2 (DuringSkillTest AnySkillTest <> exists (be a <> AssetWithCardsUnderneath AnyCards))
        $ FastAbility Free
    ]

instance RunMessage EnchantedSkull where
  runMessage msg a@(EnchantedSkull attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <- traverse (setFacedown True . toCard) =<< iid.topOfDeckN 1
      placeUnderneath attrs cards
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let n = length attrs.cardsUnderneath
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 2) iid (AnySkillValue n)
      for_ (nonEmpty attrs.cardsUnderneath) \cards -> do
        card <- setFacedown False =<< sample cards
        addToDiscard iid [card]
      pure a
    _ -> EnchantedSkull <$> liftRunMessage msg attrs
