module Arkham.Asset.Assets.BlasphemousCovenant2 (blasphemousCovenant2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.ChaosToken
import Arkham.Helpers.Window
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype BlasphemousCovenant2 = BlasphemousCovenant2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blasphemousCovenant2 :: AssetCard BlasphemousCovenant2
blasphemousCovenant2 = asset BlasphemousCovenant2 Cards.blasphemousCovenant2

instance HasAbilities BlasphemousCovenant2 where
  getAbilities (BlasphemousCovenant2 a) =
    [ controlled a 1 (DuringSkillTest AnySkillTest)
        $ triggered (RevealChaosToken #when (affectsOthers $ at_ YourLocation) #curse) (exhaust a)
    ]

instance RunMessage BlasphemousCovenant2 where
  runMessage msg a@(BlasphemousCovenant2 attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      withSkillTest \sid -> do
        skillTestModifiers
          sid
          attrs
          (ChaosTokenTarget token)
          [ChangeChaosTokenModifier (PositiveModifier 1), ReturnCursedToChaosBag]
      pure a
    _ -> BlasphemousCovenant2 <$> liftRunMessage msg attrs
