module Arkham.Asset.Assets.AncientCovenant2 (ancientCovenant2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Taboo

newtype AncientCovenant2 = AncientCovenant2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientCovenant2 :: AssetCard AncientCovenant2
ancientCovenant2 = asset AncientCovenant2 Cards.ancientCovenant2

instance HasAbilities AncientCovenant2 where
  getAbilities (AncientCovenant2 x) =
    [ controlled x 1 (DuringSkillTest SkillTestAtYourLocation)
        $ triggered
          ( ResolvesChaosToken
              #when
              (if tabooed TabooList21 x then You else affectsOthers (at_ YourLocation))
              #bless
          )
          (exhaust x)
    ]

instance RunMessage AncientCovenant2 where
  runMessage msg a@(AncientCovenant2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      withSkillTest \sid -> skillTestModifier sid iid (ChaosTokenTarget token) DoNotRevealAnotherChaosToken
      pure a
    _ -> AncientCovenant2 <$> liftRunMessage msg attrs
