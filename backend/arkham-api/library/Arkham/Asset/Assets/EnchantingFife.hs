module Arkham.Asset.Assets.EnchantingFife (enchantingFife) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype EnchantingFife = EnchantingFife AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantingFife :: AssetCard EnchantingFife
enchantingFife = asset EnchantingFife Cards.enchantingFife

{- | "Either spend 1 charge or place 1 dream": each branch gates its own
affordability, so with no charges AND no dreaming card the ability is correctly
unavailable. Both branches are labeled because the client renders only a
'LabeledCost''s label (#5730), and each label names the whole quantity.
-}
fifeCost :: AssetAttrs -> Cost
fifeCost a =
  exhaust a
    <> OrCost
      [ LabeledCost (cardI18n $ scope "enchantingFife" $ "$" <> labelKey "spendCharge")
          $ assetUseCost a Charge 1
      , LabeledCost (cardI18n $ scope "enchantingFife" $ "$" <> labelKey "placeDream")
          $ PlaceDreamCost 1
      ]

healableHorror :: AssetAttrs -> InvestigatorMatcher -> Criterion
healableHorror a who =
  AnyCriterion
    [ exists $ HealableInvestigator (a.ability 1) #horror who
    , exists $ HealableAsset (a.ability 1) #horror (AssetControlledBy who)
    ]

instance HasAbilities EnchantingFife where
  getAbilities (EnchantingFife a) =
    [ restricted a 1 (ControlsThis <> healableHorror a (affectsOthers $ colocatedWithMatch You))
        $ parleyAction (fifeCost a)
    ]

instance RunMessage EnchantingFife where
  runMessage msg a@(EnchantingFife attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      let who = affectsColocated iid
      investigators <- select $ HealableInvestigator source #horror who
      assets <- select $ HealableAsset source #horror (AssetControlledBy who)
      chooseOneM iid do
        targets investigators \i -> do
          healHorror i source 1
          mayDrawACard source i
        targets assets \aid -> do
          healHorror aid source 1
          field AssetController aid >>= traverse_ (mayDrawACard source)
      pure a
    _ -> EnchantingFife <$> liftRunMessage msg attrs

{- | "That card's controller may draw a card." A printed "may" needs its own
decline option, or the lone draw resolves unasked (#5998).
-}
mayDrawACard :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> m ()
mayDrawACard source iid = chooseOneM iid $ cardI18n $ scope "enchantingFife" do
  labeled "drawACard" $ drawCards iid source 1
  labeled "doNotDrawACard" nothing
