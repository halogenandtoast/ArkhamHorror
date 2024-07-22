module Arkham.Asset.Cards.MarinersCompass2 (marinersCompass2, MarinersCompass2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigate
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype MarinersCompass2 = MarinersCompass2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marinersCompass2 :: AssetCard MarinersCompass2
marinersCompass2 = asset MarinersCompass2 Cards.marinersCompass2

instance HasAbilities MarinersCompass2 where
  getAbilities (MarinersCompass2 x) =
    [ restrictedAbility x 1 ControlsThis $ investigateAction (exhaust x)
    , limitedAbility (PlayerLimit PerTestOrAbility 3)
        $ controlledAbility x 2 (DuringSkillTest UsingThis)
        $ FastAbility (ResourceCost 1)
    ]

instance RunMessage MarinersCompass2 where
  runMessage msg a@(MarinersCompass2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      pushM $ mkInvestigate sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid attrs iid (SkillModifier #intellect 1)
      pure a
    FailedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      pushWhen attrs.exhausted $ ready attrs
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      noResources <- fieldNone InvestigatorResources iid
      withSkillTest \sid ->
        pushWhen noResources $ skillTestModifier sid attrs iid (DiscoveredClues 1)
      pure a
    _ -> MarinersCompass2 <$> runMessage msg attrs
