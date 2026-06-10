module Arkham.Asset.Assets.AncientAnkh (ancientAnkh) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (getSkillTestId)
import Arkham.Matcher
import Arkham.Window (windowType)
import Arkham.Window qualified as Window

newtype AncientAnkh = AncientAnkh AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientAnkh :: AssetCard AncientAnkh
ancientAnkh = assetWith AncientAnkh Cards.ancientAnkh (sanityL ?~ 3)

instance HasAbilities AncientAnkh where
  getAbilities (AncientAnkh a) =
    [ controlled a 1 (DuringSkillTest $ SkillTestOfInvestigator $ InvestigatorAt YourLocation)
        $ triggered
          (WouldHaveSkillTestResult #when (InvestigatorAt YourLocation) AnySkillTest (FailureResult $ atLeast 2))
          (assetUseCost a Charge 1)
    ]

instance RunMessage AncientAnkh where
  runMessage msg a@(AncientAnkh attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      let mn = listToMaybe [x | (windowType -> Window.WouldFailSkillTest _ x) <- ws]
      for_ mn \n -> do
        whenJustM getSkillTestId \sid ->
          skillTestModifier sid (attrs.ability 1) (SkillTestTarget sid) (SkillTestResultValueModifier (n - 1))
      pure a
    _ -> AncientAnkh <$> liftRunMessage msg attrs
