module Arkham.Homebrew.CircusExMortis.Stories.HiddenInPlainSight (hiddenInPlainSight) where

import Arkham.Ability
import Arkham.GameValue (GameValue (..))
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTest, withSkillTest)
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (campaignI18n)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Story.Import.Lifted

newtype HiddenInPlainSight = HiddenInPlainSight StoryAttrs
  deriving anyclass IsStory
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Starts Kidnapped Citizen (b) side up; flips to the story side and back.
hiddenInPlainSight :: StoryCard HiddenInPlainSight
hiddenInPlainSight = storyWith HiddenInPlainSight Cards.hiddenInPlainSight (flippedL .~ True)

instance HasModifiersFor HiddenInPlainSight where
  getModifiersFor (HiddenInPlainSight attrs) = do
    whenJustM getSkillTest \st -> maybeModified_ attrs (SkillTestTarget st.id) do
      guard $ isAbilitySource attrs 2 st.source
      n <- lift $ perPlayer 1
      pure [Difficulty n]

instance HasAbilities HiddenInPlainSight where
  getAbilities (HiddenInPlainSight attrs)
    | attrs.flipped =
        [restricted attrs 1 OnSameLocation $ freeTrigger (GroupClueCost (PerPlayer 1) YourLocation)]
    | otherwise =
        [ skillTestAbility $ restricted attrs 2 OnSameLocation actionAbility
        , mkAbility attrs 3
            $ SilentForcedAbility
            $ InitiatedSkillTest #after You AnySkillType AnySkillTestValue
            $ SkillTestSourceMatches (SourceIs (attrs.ability 2))
        ]

instance RunMessage HiddenInPlainSight where
  runMessage msg s@(HiddenInPlainSight attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      pure . HiddenInPlainSight $ attrs & flippedL .~ False
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#willpower, #intellect] \sType ->
          skillLabeled sType $ beginSkillTest sid iid (attrs.ability 2) attrs sType (Fixed 3)
      pure s
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      n <- perPlayer 1
      chooseOneM iid $ campaignI18n $ scope "hiddenInPlainSight" do
        labeled "spendCluesToAutoSucceed" do
          withLocationOf attrs.placement \loc ->
            withCost iid (GroupClueCost (PerPlayer 1) (LocationWithId loc))
              $ withSkillTest (skillTestAutomaticallySucceeds (attrs.ability 2))
        countVar n $ labeled "doNotSpendClues" nothing
      pure s
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      addToVictory iid attrs
      pure . HiddenInPlainSight $ attrs & flippedL .~ True
    FailedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      assignHorror iid attrs 1
      pure s
    _ -> HiddenInPlainSight <$> liftRunMessage msg attrs
