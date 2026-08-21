module Arkham.Asset.Assets.FangOfZburamoarte (fangOfZburamoarte) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.ChaosToken
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFace)
import Arkham.Helpers.SkillTest (withSkillTest, withSkillTestSource)
import Arkham.Modifier

newtype FangOfZburamoarte = FangOfZburamoarte AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fangOfZburamoarte :: AssetCard FangOfZburamoarte
fangOfZburamoarte = asset FangOfZburamoarte Cards.fangOfZburamoarte

getPaidUse :: Payment -> Bool
getPaidUse = (> 0) . totalUsesPayment

instance HasAbilities FangOfZburamoarte where
  getAbilities (FangOfZburamoarte a) =
    [ withAdditionalCost (UpTo (Fixed 1) $ assetUseCost a Charge 1)
        $ restricted a 1 ControlsThis fightAction_
    ]

instance RunMessage FangOfZburamoarte where
  runMessage msg a@(FangOfZburamoarte attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (getPaidUse -> paidUse) -> do
      sid <- getRandom
      let source = attrs.ability 1
      skillTestModifier sid source iid (SkillModifier #combat 3)
      when paidUse $ skillTestModifier sid source sid RevealAnotherChaosToken
      chooseFightEnemy sid iid source
      pure a
    RevealChaosToken _ iid token -> do
      withSkillTestSource \source -> when (isAbilitySource attrs 1 source) do
        faces <- getModifiedChaosTokenFace token
        when (BloodToken `elem` faces) do
          withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
      pure a
    _ -> FangOfZburamoarte <$> liftRunMessage msg attrs
