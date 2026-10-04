module Arkham.Homebrew.AgainstTheWendigo.Stories.HanninahsGold (hanninahsGold) where

import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (scenarioI18n)
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (record)
import Arkham.Story.Import.Lifted

newtype HanninahsGold = HanninahsGold StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hanninahsGold :: StoryCard HanninahsGold
hanninahsGold = story HanninahsGold Cards.hanninahsGold

{- | "Choose if you want to listen to the prospector and attempt to leave the
mountains (Choice 1), or if you are going to see the gold vein (Choice 2)."

Both choices are a single test taken at the Mad Prospector, so rather than
attaching the card and granting an ability, the choice is made and the test
taken when the story is read. Which choice was taken rides in the story's meta,
because the two tests differ only in skill and in what a failure costs.
-}
instance RunMessage HanninahsGold where
  runMessage msg s@(HanninahsGold attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      sid <- getRandom
      chooseOneM iid $ scenarioI18n $ scope "hanninahsGold" do
        labeled "listenToTheProspector" do
          push $ HandleTargetChoice iid (toSource attrs) (toTarget attrs)
          beginSkillTest sid iid attrs iid #intellect (Fixed 3)
        labeled "seeTheGoldVein" $ beginSkillTest sid iid attrs iid #combat (Fixed 3)
      pure s
    -- Only choice 1 announces itself; the meta starts out False for choice 2.
    HandleTargetChoice _ (isSource attrs -> True) (isTarget attrs -> True) ->
      pure $ HanninahsGold $ setMeta True attrs
    PassedThisSkillTest _ (isSource attrs -> True) -> do
      if toResultDefault False attrs.meta
        then record YouSavedTheGoldProspector
        else record YouHaveFoundHanninahsGold
      removeStory attrs
      pure s
    -- "For each point you fail by, take 1 horror" (choice 1) or "1 damage" (choice 2).
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n -> do
      if toResultDefault False attrs.meta
        then assignHorror iid attrs n
        else assignDamage iid attrs n
      pure s
    _ -> HanninahsGold <$> liftRunMessage msg attrs
