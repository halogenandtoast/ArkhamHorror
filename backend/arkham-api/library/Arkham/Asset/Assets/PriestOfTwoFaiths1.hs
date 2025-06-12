module Arkham.Asset.Assets.PriestOfTwoFaiths1 (priestOfTwoFaiths1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.ChaosBag
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype PriestOfTwoFaiths1 = PriestOfTwoFaiths1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

priestOfTwoFaiths1 :: AssetCard PriestOfTwoFaiths1
priestOfTwoFaiths1 = ally PriestOfTwoFaiths1 Cards.priestOfTwoFaiths1 (2, 2)

instance HasAbilities PriestOfTwoFaiths1 where
  getAbilities (PriestOfTwoFaiths1 x) =
    [ controlled x 1 HasRemainingBlessTokens $ freeReaction $ AssetEntersPlay #when (be x)
    , restricted x 2 ControlsThis $ forced $ PhaseEnds #when #upkeep
    ]

instance RunMessage PriestOfTwoFaiths1 where
  runMessage msg a@(PriestOfTwoFaiths1 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- min 3 <$> getRemainingBlessTokens
      repeated n $ addChaosToken #bless
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      n <- getRemainingCurseTokens
      chooseOrRunOneM iid do
        when (n > 0) $ labeled "Add 1 {curse} token to the chaos bag" $ addCurseTokens (Just iid) 1
        labeled "Discard Priest of Two Faiths" $ toDiscardBy iid (attrs.ability 2) attrs
      pure a
    _ -> PriestOfTwoFaiths1 <$> liftRunMessage msg attrs
