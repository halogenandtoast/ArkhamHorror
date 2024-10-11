module Arkham.Asset.Assets.AzureFlame5 (azureFlame5, AzureFlame5 (..)) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.ChaosToken
import Arkham.Fight
import Arkham.Matcher
import Arkham.Modifier

newtype AzureFlame5 = AzureFlame5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

azureFlame5 :: AssetCard AzureFlame5
azureFlame5 = asset AzureFlame5 Cards.azureFlame5

instance HasAbilities AzureFlame5 where
  getAbilities (AzureFlame5 a) =
    [ restricted a 1 ControlsThis
        $ ActionAbilityWithSkill [#fight] #willpower
        $ Costs [ActionCost 1, assetUseCost a Charge 1]
    ]

instance RunMessage AzureFlame5 where
  runMessage msg a@(AzureFlame5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid attrs iid [DamageDealt 2, SkillModifier #willpower 3]
      createCardEffect Cards.azureFlame5 Nothing source sid
      onRevealChaosTokenEffect sid (mapOneOf ChaosTokenFaceIs [ElderSign, PlusOne, Zero]) source sid do
        assignDamage iid source 2
      aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight sid iid source)
      pure a
    _ -> AzureFlame5 <$> liftRunMessage msg attrs
