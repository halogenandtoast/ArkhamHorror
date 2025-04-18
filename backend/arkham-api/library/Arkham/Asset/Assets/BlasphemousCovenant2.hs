module Arkham.Asset.Assets.BlasphemousCovenant2 (blasphemousCovenant2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Prelude

newtype BlasphemousCovenant2 = BlasphemousCovenant2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blasphemousCovenant2 :: AssetCard BlasphemousCovenant2
blasphemousCovenant2 = asset BlasphemousCovenant2 Cards.blasphemousCovenant2

instance HasAbilities BlasphemousCovenant2 where
  getAbilities (BlasphemousCovenant2 a) =
    [ restricted a 1 ControlsThis
        $ triggered
          (RevealChaosToken #when (affectsOthers $ InvestigatorAt YourLocation) #curse)
          (exhaust a)
    ]

instance RunMessage BlasphemousCovenant2 where
  runMessage msg a@(BlasphemousCovenant2 attrs) = case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      withSkillTest \sid -> do
        pushM
          $ skillTestModifiers
            sid
            attrs
            (ChaosTokenTarget token)
            [ChangeChaosTokenModifier (PositiveModifier 1), ReturnCursedToChaosBag]
      pure a
    _ -> BlasphemousCovenant2 <$> runMessage msg attrs
