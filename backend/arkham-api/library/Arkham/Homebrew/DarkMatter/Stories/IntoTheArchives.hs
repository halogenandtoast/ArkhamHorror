module Arkham.Homebrew.DarkMatter.Stories.IntoTheArchives (intoTheArchives) where

import Arkham.Helpers.Investigator
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (addMemories)
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Story.Import.Lifted

newtype IntoTheArchives = IntoTheArchives StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheArchives :: StoryCard IntoTheArchives
intoTheArchives = story IntoTheArchives Cards.intoTheArchives

instance RunMessage IntoTheArchives where
  runMessage msg s@(IntoTheArchives attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      selectEach (colocatedWith iid) (`addMemories` 1)
      whenM (canHaveHorrorHealed attrs iid) do
        hrr <- field InvestigatorHorror iid
        withI18n $ chooseAmount' iid "horror" "$horror" 0 hrr attrs
      addToVictory iid attrs
      pure s
    ResolveAmounts iid (getChoiceAmount "$horror" -> n) (isTarget attrs -> True) -> do
      when (n > 0) $ healHorror iid attrs n
      pure s
    _ -> IntoTheArchives <$> liftRunMessage msg attrs
