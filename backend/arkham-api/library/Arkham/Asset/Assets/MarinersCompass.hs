module Arkham.Asset.Assets.MarinersCompass (marinersCompass) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Modifier
import Arkham.Investigate
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted (provideSkillTestResultOption)
import Arkham.Projection
import Arkham.SkillTestResult

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
        $ FastAbility (ResourceCost 1)
    ]

instance RunMessage MarinersCompass where
  runMessage msg a@(MarinersCompass attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigate sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid ->
        skillTestModifier sid attrs iid (SkillModifier #intellect 1)
      pure a
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (isAbilitySource attrs 1 st.source) do
          case st.result of
            SucceededBy {} -> do
              noResources <- fieldNone InvestigatorResources st.investigator
              when noResources do
                provideSkillTestResultOption attrs exclusions "Mariner's Compass: Discover Additional Clue" do
                  discoverAtYourLocation st.investigator attrs 1
            _ -> pure ()
      pure a
    _ -> MarinersCompass <$> liftRunMessage msg attrs
