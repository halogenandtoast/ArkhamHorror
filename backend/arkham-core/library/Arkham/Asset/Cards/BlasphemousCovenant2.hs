module Arkham.Asset.Cards.BlasphemousCovenant2 (blasphemousCovenant2, BlasphemousCovenant2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Matcher
import Arkham.Prelude

newtype BlasphemousCovenant2 = BlasphemousCovenant2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blasphemousCovenant2 :: AssetCard BlasphemousCovenant2
blasphemousCovenant2 = asset BlasphemousCovenant2 Cards.blasphemousCovenant2

instance HasAbilities BlasphemousCovenant2 where
  getAbilities (BlasphemousCovenant2 a) =
    [ restrictedAbility
        a
        1
        ControlsThis
        $ ReactionAbility
          (RevealChaosToken #when (affectsOthers $ InvestigatorAt YourLocation) #curse)
          (exhaust a)
    ]

instance RunMessage BlasphemousCovenant2 where
  runMessage msg a@(BlasphemousCovenant2 attrs) = case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      push
        $ skillTestModifiers
          attrs
          (ChaosTokenTarget token)
          [ChangeChaosTokenModifier (PositiveModifier 1), ReturnCursedToChaosBag]
      pure a
    _ -> BlasphemousCovenant2 <$> runMessage msg attrs
