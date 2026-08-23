module Arkham.Asset.Assets.SanguineSong (sanguineSong) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.ChaosToken
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFace)
import Arkham.Helpers.SkillTest (withSkillTest, withSkillTestSource)
import Arkham.Helpers.SkillTest.Lifted (investigate_)
import Arkham.Modifier

newtype SanguineSong = SanguineSong AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sanguineSong :: AssetCard SanguineSong
sanguineSong = asset SanguineSong Cards.sanguineSong

getPaidUse :: Payment -> Bool
getPaidUse = (> 0) . totalUsesPayment

instance HasAbilities SanguineSong where
  getAbilities (SanguineSong a) =
    [ withAdditionalCost (UpTo (Fixed 1) $ assetUseCost a Charge 1)
        $ investigateAbility a 1 mempty ControlsThis
    ]

instance RunMessage SanguineSong where
  runMessage msg a@(SanguineSong attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (getPaidUse -> paidUse) -> do
      sid <- getRandom
      let source = attrs.ability 1
      skillTestModifier sid source iid (SkillModifier #intellect 3)
      when paidUse $ skillTestModifier sid source sid RevealAnotherChaosToken
      investigate_ sid iid source
      pure a
    RevealChaosToken _ iid token -> do
      withSkillTestSource \source -> when (isAbilitySource attrs 1 source) do
        faces <- getModifiedChaosTokenFace token
        when (BloodToken `elem` faces) do
          withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (DiscoveredClues 1)
      pure a
    _ -> SanguineSong <$> liftRunMessage msg attrs
