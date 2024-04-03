module Arkham.Asset.Cards.AncientCovenant2 (ancientCovenant2, AncientCovenant2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype AncientCovenant2 = AncientCovenant2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientCovenant2 :: AssetCard AncientCovenant2
ancientCovenant2 = asset AncientCovenant2 Cards.ancientCovenant2

instance HasAbilities AncientCovenant2 where
  getAbilities (AncientCovenant2 x) =
    [ controlledAbility x 1 (DuringSkillTest SkillTestAtYourLocation)
        $ ReactionAbility
          (ResolvesChaosToken #when (affectsOthers $ InvestigatorAt YourLocation) #bless)
          (exhaust x)
    ]

instance RunMessage AncientCovenant2 where
  runMessage msg a@(AncientCovenant2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      push $ skillTestModifier iid (ChaosTokenTarget token) DoNotRevealAnotherChaosToken
      pure a
    _ -> AncientCovenant2 <$> runMessage msg attrs
