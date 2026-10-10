module Arkham.Homebrew.AgesUnwound.Stories.WindowOfOpportunity (windowOfOpportunity) where

import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (completeTask)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Story.Import.Lifted

newtype WindowOfOpportunity = WindowOfOpportunity StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The reverse of /Brainwashed Expedition/ (@:ages-unwound:146@).
windowOfOpportunity :: StoryCard WindowOfOpportunity
windowOfOpportunity = story WindowOfOpportunity Cards.windowOfOpportunity

{- | "You must decide (choose one):
- /You retreat, while you still have that option./ Each investigator at your
  location may move to a connecting location. Flip this card back over and
  restore it to 1[per_investigator] health.
- /You move toward the dig site, determined to seal this thing back where it
  came from./ An investigator at your location tests [willpower] (3). If they
  succeed, complete The Tunguska Event, flip this card back over and add it to
  the victory display. Otherwise, flip this card back over and restore it to
  1[per_investigator] health.

"Restore it to 1[per_investigator] health" means the host comes back, which is
why Brainwashed Expedition cancels its own defeat before reading this: healing
all damage puts it back at full health, and only the victory branch takes it out
of play.
-}
instance RunMessage WindowOfOpportunity where
  runMessage msg s@(WindowOfOpportunity attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      here <- select $ colocatedWith iid
      chooseOneM iid $ campaignI18n do
        labeled "windowOfOpportunity.retreat" do
          for_ here \iid' -> do
            accessible <- getAccessibleLocations iid' attrs
            chooseOneM iid' do
              withI18n skip_
              targets accessible $ moveTo attrs iid'
          restoreHost attrs
        labeled "windowOfOpportunity.digSite" $ chooseOrRunOneM iid $ targets here \iid' -> do
          sid <- getRandom
          beginSkillTest sid iid' attrs attrs #willpower (Fixed 3)
      pure s
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      completeTask Treacheries.theTunguskaEvent
      for_ (storyOtherSide attrs) (addToVictory iid)
      pure s
    FailedThisSkillTest _iid (isSource attrs -> True) -> do
      restoreHost attrs
      pure s
    _ -> WindowOfOpportunity <$> liftRunMessage msg attrs

-- | "Restore it to 1[per_investigator] health."
restoreHost :: ReverseQueue m => StoryAttrs -> m ()
restoreHost attrs = for_ (storyOtherSide attrs) (healAllDamage attrs)
