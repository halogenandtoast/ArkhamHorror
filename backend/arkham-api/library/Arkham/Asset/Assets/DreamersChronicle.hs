module Arkham.Asset.Assets.DreamersChronicle (dreamersChronicle, dreamersChronicleEffect) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Effect.Import
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype DreamersChronicle = DreamersChronicle AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamersChronicle :: AssetCard DreamersChronicle
dreamersChronicle = asset DreamersChronicle Cards.dreamersChronicle

instance HasAbilities DreamersChronicle where
  getAbilities (DreamersChronicle a) =
    [investigateAbility a 1 (assetUseCost a Secret 1) ControlsThis]

instance RunMessage DreamersChronicle where
  runMessage msg a@(DreamersChronicle attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      createCardEffect Cards.dreamersChronicle Nothing (attrs.ability 1) iid
      investigate sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      canDiscover <-
        selectAny $ locationWithInvestigator iid <> LocationWithDiscoverableCluesBy (InvestigatorWithId iid)
      when canDiscover do
        additionalSkillTestOption "Dreamer's Chronicle" do
          chooseOneM iid do
            labeled "Take 1 horror to discover 1 additional clue at your location" do
              assignHorror iid (attrs.ability 1) 1
              discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
            labeled "Skip" nothing
      pure a
    _ -> DreamersChronicle <$> liftRunMessage msg attrs

newtype DreamersChronicleEffect = DreamersChronicleEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamersChronicleEffect :: EffectArgs -> DreamersChronicleEffect
dreamersChronicleEffect = cardEffect DreamersChronicleEffect Cards.dreamersChronicle

instance RunMessage DreamersChronicleEffect where
  runMessage msg e@(DreamersChronicleEffect attrs) = runQueueT $ case msg of
    Do (CommitCard iid card) | Just iid == attrs.target.investigator -> do
      withSkillTest \sid ->
        skillTestModifier sid attrs card (AddSkillIcons [#wild])
      disable attrs
      pure e
    SkillTestEnds {} -> disableReturn e
    _ -> DreamersChronicleEffect <$> liftRunMessage msg attrs
