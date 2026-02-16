module Arkham.Asset.Assets.MarinersCompass (marinersCompass) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection

newtype MarinersCompass = MarinersCompass AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marinersCompass :: AssetCard MarinersCompass
marinersCompass = asset MarinersCompass Cards.marinersCompass

instance HasAbilities MarinersCompass where
  getAbilities (MarinersCompass x) =
    [ controlled_ x 1 $ investigateAction (exhaust x)
    , limited (PlayerLimit PerTestOrAbility 3)
        $ controlled x 2 (DuringSkillTest UsingThis)
        $ freeTrigger (ResourceCost 1)
    ]

instance RunMessage MarinersCompass where
  runMessage msg a@(MarinersCompass attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigate sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid attrs iid (SkillModifier #intellect 1)
      pure a
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      skillTestResultOption "Mariner's Compass" $ doStep 1 msg
      pure a
    DoStep 1 (PassedThisSkillTest iid (isAbilitySource attrs 1 -> True)) -> do
      noResources <- fieldNone InvestigatorResources iid
      when noResources do
        withSkillTest \sid -> skillTestModifier sid attrs iid (DiscoveredClues 1)
      pure a
    _ -> MarinersCompass <$> liftRunMessage msg attrs
