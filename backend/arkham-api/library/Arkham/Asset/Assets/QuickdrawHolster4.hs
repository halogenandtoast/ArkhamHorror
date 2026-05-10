module Arkham.Asset.Assets.QuickdrawHolster4 (quickdrawHolster4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Ability (getCanPerformAbility)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Placement
import Arkham.Window (defaultWindows)

newtype QuickdrawHolster4 = QuickdrawHolster4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickdrawHolster4 :: AssetCard QuickdrawHolster4
quickdrawHolster4 = asset QuickdrawHolster4 Cards.quickdrawHolster4

instance HasModifiersFor QuickdrawHolster4 where
  getModifiersFor (QuickdrawHolster4 a) = modifySelect a (AssetAttachedToAsset (be a)) [DoNotTakeUpSlot #hand]

instance HasAbilities QuickdrawHolster4 where
  getAbilities (QuickdrawHolster4 x) =
    [ (cardI18n $ withI18nTooltip "quickdrawHolster4.fastChooseA")
        $ controlled
          x
          1
          ( exists
              $ #firearm
              <> AssetInSingleHand
              <> AssetControlledBy You
              <> not_ (AssetAttachedToAsset $ be x)
          )
          (FastAbility Free)
    , (cardI18n $ withI18nTooltip "quickdrawHolster4.fastExhaustQuickdraw")
        $ controlled
          x
          2
          ( exists
              $ AssetAttachedToAsset (be x)
              <> AssetWithPerformableAbility (AbilityIsAction #fight) [ActionCostSetToModifier 0]
          )
          (FastAbility (exhaust x))
    ]

instance RunMessage QuickdrawHolster4 where
  runMessage msg a@(QuickdrawHolster4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <-
        select
          $ #firearm
          <> AssetInSingleHand
          <> assetControlledBy iid
          <> not_ (AssetAttachedToAsset $ be attrs)
      hasAttached <- selectOne $ AssetAttachedToAsset (be attrs)
      let
        orSwap msg' = case hasAttached of
          Nothing -> [msg']
          Just aid' -> [msg', PlaceAsset aid' (InPlayArea iid)]
      chooseOrRunOne
        iid
        [ targetLabel x (orSwap $ PlaceAsset x $ AttachedToAsset attrs.id (Just $ InPlayArea iid))
        | x <- assets
        ]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      maid <- selectOne $ AssetAttachedToAsset (AssetWithId attrs.id)
      for_ maid \aid -> do
        let nullifyActionCost ab = applyAbilityModifiers ab [ActionCostSetToModifier 0]
        abilities <- selectMap nullifyActionCost $ AbilityIsAction #fight <> AssetAbility (AssetWithId aid)
        abilities' <- filterM (getCanPerformAbility iid (defaultWindows iid)) abilities
        chooseOne iid [AbilityLabel iid ab [] [] [] | ab <- abilities']
      pure a
    _ -> QuickdrawHolster4 <$> liftRunMessage msg attrs
