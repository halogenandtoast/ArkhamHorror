module Arkham.Homebrew.CircusExMortis.Stories.UnderLockAndKey (underLockAndKey) where

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

newtype UnderLockAndKey = UnderLockAndKey StoryAttrs
  deriving anyclass IsStory
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Starts Kidnapped Citizen (b) side up; flips to the story side and back.
underLockAndKey :: StoryCard UnderLockAndKey
underLockAndKey = storyWith UnderLockAndKey Cards.underLockAndKey (flippedL .~ True)

instance HasModifiersFor UnderLockAndKey where
  getModifiersFor (UnderLockAndKey attrs) = do
    whenJustM getSkillTest \st -> maybeModified_ attrs (SkillTestTarget st.id) do
      guard $ isAbilitySource attrs 2 st.source
      n <- lift $ perPlayer 1
      pure [Difficulty n]

instance HasAbilities UnderLockAndKey where
  getAbilities (UnderLockAndKey attrs)
    | attrs.flipped =
        [restricted attrs 1 OnSameLocation $ freeTrigger (GroupClueCost (PerPlayer 1) YourLocation)]
    | otherwise =
        [ skillTestAbility $ restricted attrs 2 OnSameLocation actionAbility
        , mkAbility attrs 3
            $ SilentForcedAbility
            $ InitiatedSkillTest #after You AnySkillType AnySkillTestValue
            $ SkillTestSourceMatches (SourceIs (attrs.ability 2))
        ]

instance RunMessage UnderLockAndKey where
  runMessage msg s@(UnderLockAndKey attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      pure . UnderLockAndKey $ attrs & flippedL .~ False
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#combat, #agility] \sType ->
          skillLabeled sType $ beginSkillTest sid iid (attrs.ability 2) attrs sType (Fixed 3)
      pure s
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      n <- perPlayer 1
      chooseOneM iid $ campaignI18n $ scope "underLockAndKey" do
        labeled' "spendCluesToAutoSucceed" do
          withLocationOf attrs.placement \loc ->
            withCost iid (GroupClueCost (PerPlayer 1) (LocationWithId loc))
              $ withSkillTest (skillTestAutomaticallySucceeds (attrs.ability 2))
        countVar n $ labeled' "doNotSpendClues" nothing
      pure s
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      addToVictory iid attrs
      pure . UnderLockAndKey $ attrs & flippedL .~ True
    FailedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      assignDamage iid attrs 1
      pure s
    _ -> UnderLockAndKey <$> liftRunMessage msg attrs
