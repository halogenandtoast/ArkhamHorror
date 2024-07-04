module Arkham.Asset.Cards.QuickdrawHolster4 (quickdrawHolster4, QuickdrawHolster4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Game.Helpers (getCanPerformAbility)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Placement
import Arkham.Window (defaultWindows)

newtype QuickdrawHolster4 = QuickdrawHolster4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickdrawHolster4 :: AssetCard QuickdrawHolster4
quickdrawHolster4 = asset QuickdrawHolster4 Cards.quickdrawHolster4

instance HasAbilities QuickdrawHolster4 where
  getAbilities (QuickdrawHolster4 x) =
    [ withTooltip
        "{fast}: Choose a _Firearm_ asset taking up only 1 hand slot in your play area. Attach it to Quickdraw Holster, or switch it with the attached asset. The attached asset takes up no hand slots. (Limit 1 attached asset.)"
        $ controlledAbility
          x
          1
          ( exists
              $ #firearm
              <> AssetInSingleHand
              <> AssetControlledBy You
              <> not_ (AssetAttachedToAsset $ be x)
          )
          (FastAbility Free)
    , withTooltip
        "{fast} Exhaust Quickdraw Holster: Perform a fight action on the attached asset without paying its {action} cost."
        $ controlledAbility
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
        chooseOne iid [AbilityLabel iid ab [] [] | ab <- abilities']
      pure a
    _ -> QuickdrawHolster4 <$> liftRunMessage msg attrs
