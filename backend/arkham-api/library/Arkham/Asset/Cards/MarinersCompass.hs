module Arkham.Asset.Cards.MarinersCompass (marinersCompass, MarinersCompass (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigate
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype MarinersCompass = MarinersCompass AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marinersCompass :: AssetCard MarinersCompass
marinersCompass = asset MarinersCompass Cards.marinersCompass

instance HasAbilities MarinersCompass where
  getAbilities (MarinersCompass x) =
    [ restrictedAbility x 1 ControlsThis $ investigateAction (exhaust x)
    , limitedAbility (PlayerLimit PerTestOrAbility 3)
        $ controlledAbility x 2 (DuringSkillTest UsingThis)
        $ FastAbility (ResourceCost 1)
    ]

instance RunMessage MarinersCompass where
  runMessage msg a@(MarinersCompass attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      pushM $ mkInvestigate sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid attrs iid (SkillModifier #intellect 1)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      noResources <- fieldNone InvestigatorResources iid
      withSkillTest \sid ->
        pushWhen noResources $ skillTestModifier sid attrs iid (DiscoveredClues 1)
      pure a
    _ -> MarinersCompass <$> runMessage msg attrs
