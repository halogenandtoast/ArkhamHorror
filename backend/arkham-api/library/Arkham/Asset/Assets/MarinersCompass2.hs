module Arkham.Asset.Assets.MarinersCompass2 (marinersCompass2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.GameValue
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype MarinersCompass2 = MarinersCompass2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marinersCompass2 :: AssetCard MarinersCompass2
marinersCompass2 = asset MarinersCompass2 Cards.marinersCompass2

instance HasAbilities MarinersCompass2 where
  getAbilities (MarinersCompass2 x) =
    [ controlled_ x 1 $ investigateAction (exhaust x)
    , limited (PlayerLimit PerTestOrAbility 3)
        $ controlled x 2 (DuringSkillTest UsingThis)
        $ freeTrigger (ResourceCost 1)
    ]

instance RunMessage MarinersCompass2 where
  runMessage msg a@(MarinersCompass2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigate sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid attrs iid (SkillModifier #intellect 1)
      pure a
    FailedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      when attrs.exhausted $ ready attrs
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      withSkillTest \sid ->
        blockingSkillTestOptionWithCriteria
          (exists $ InvestigatorWithId iid <> InvestigatorWithResources (EqualTo $ Static 0))
          "Mariner's Compass (2)"
          $ skillTestModifier sid attrs iid (DiscoveredClues 1)
      pure a
    _ -> MarinersCompass2 <$> liftRunMessage msg attrs
