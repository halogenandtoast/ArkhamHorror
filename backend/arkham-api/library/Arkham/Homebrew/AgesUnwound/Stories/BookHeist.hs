module Arkham.Homebrew.AgesUnwound.Stories.BookHeist (bookHeist) where

import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (completeTask, takeControlOfSetAsideRewardAsset)
import Arkham.Location.Types (Field (LocationClues))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection
import Arkham.Story.Import.Lifted

newtype BookHeist = BookHeist StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The reverse of /The British Library/ (@:ages-unwound:143@).
bookHeist :: StoryCard BookHeist
bookHeist = story BookHeist Cards.bookHeist

{- | "You must decide (choose one):
- /Wait for your moment, then swoop in and grab it./ Investigators at your
  location spend 3 clues, as a group. Test [agility] (0). This test gets +1
  difficulty for each clue on the British Library. If you succeed, put the
  set-aside Chronal Atlas asset into play under your control and complete A
  Treasure Unearthed. For the remainder of this scenario, Chronal Atlas does not
  take up a hand slot. Then, flip this card back over; if you succeeded, for the
  remainder of the game, it cannot be flipped over again.
- /It's just not worth the risk./ Flip this card back over."

Flipping back is what happens by default -- the story is removed after it
resolves and The British Library is still there. The "cannot be flipped over
again" lock is read off the completed Task by the location itself, which is the
same condition: succeeding is exactly what completes /A Treasure Unearthed/.
-}
instance RunMessage BookHeist where
  runMessage msg s@(BookHeist attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      clues <- selectMaybeM 0 (locationIs Locations.theBritishLibrary) (field LocationClues)
      here <- select $ colocatedWith iid
      groupClues <- getSpendableClueCount here
      chooseOneM iid $ campaignI18n do
        labeledValidate (groupClues >= 3) "bookHeist.steal" do
          spendCluesAsAGroup here 3
          sid <- getRandom
          skillTestModifier sid attrs sid (Difficulty clues)
          beginSkillTest sid iid attrs attrs #agility (Fixed 0)
        labeled "bookHeist.notWorthTheRisk" nothing
      pure s
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      takeControlOfSetAsideRewardAsset attrs iid Assets.chronalAtlas #hand
      completeTask Treacheries.aTreasureUnearthed
      pure s
    _ -> BookHeist <$> liftRunMessage msg attrs
