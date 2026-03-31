module Arkham.Asset.Assets.SacrificialDoll (sacrificialDoll) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Helpers.SkillTest (withSkillTest, withSkillTestInvestigator)
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype SacrificialDoll = SacrificialDoll AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sacrificialDoll :: AssetCard SacrificialDoll
sacrificialDoll = assetWith SacrificialDoll Cards.sacrificialDoll (healthL ?~ 1)

instance HasModifiersFor SacrificialDoll where
  getModifiersFor (SacrificialDoll a) = do
    for_ a.controller \iid -> do
      remainingHealth <- field InvestigatorRemainingHealth iid
      modifySelfWhen a (remainingHealth <= 3) [DoNotTakeUpSlot #hand]

instance HasAbilities SacrificialDoll where
  getAbilities (SacrificialDoll a) =
    [ controlled a 1 (DuringSkillTest $ YourSkillTest AnySkillTest)
        $ triggered
          (RevealChaosToken #after You (not_ #autofail))
          (DirectDamageCost (toSource a) You 1 <> exhaust a)
    ]

instance RunMessage SacrificialDoll where
  runMessage msg a@(SacrificialDoll attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      cancelChaosToken (attrs.ability 1) iid token
      withSkillTestInvestigator \iid' -> do
        withSkillTest \sid ->
          skillTestModifier sid (attrs.ability 1) sid (CancelAnyChaosTokenAndDrawAnother (not_ IsSymbol))
        drawAnotherChaosToken iid'
      pure a
    _ -> do
      a'@(SacrificialDoll attrs') <- SacrificialDoll <$> liftRunMessage msg attrs
      let wasOccupying = toResultDefault True attrs.meta
      case attrs'.controller of
        Nothing -> pure a'
        Just iid -> do
          remainingHealth <- field InvestigatorRemainingHealth iid
          let isFreed = remainingHealth <= 3
          if wasOccupying == isFreed
            then do
              priority $ push $ RefillSlots iid []
              pure $ SacrificialDoll (setMeta (not isFreed) attrs')
            else pure a'
