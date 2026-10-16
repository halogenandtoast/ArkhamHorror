{- | Act 1, The Madman's Notebook.

Advancing lays out the arrangement: the printing of each duplicated location
that setup kept out, shuffled, in a line beneath the act deck. They are real
locations, isolated by the modifiers the scenario publishes on the @arrangement@
labels, so placing them is all this does -- that and standing Adapted Formula up
next to the act deck.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Acts.TheMadmansNotebook (
  theMadmansNotebook,
) where

import Arkham.Act.Import.Lifted
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Stories qualified as Stories
import Arkham.Matcher
import Arkham.Message.Story (StoryMessage (PlaceStory))
import Arkham.Placement (Placement (NextToAct))

newtype TheMadmansNotebook = TheMadmansNotebook ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theMadmansNotebook :: ActCard TheMadmansNotebook
theMadmansNotebook =
  act
    (1, A)
    TheMadmansNotebook
    Cards.theMadmansNotebook
    (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance RunMessage TheMadmansNotebook where
  runMessage msg a@(TheMadmansNotebook attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- "each of the set-aside locations" is only the duplicates; the other
      -- set-aside cards are the formulas, the drug and Your Other Half.
      placeRandomLocationGroup "arrangement" =<< getSetAsideCardsMatching #location
      formula <- getSetAsideCard Stories.adaptedFormula
      -- 'PlaceStory' does not clear the set-aside pile on its own.
      obtainCard formula
      push $ PlaceStory formula NextToAct
      advanceActDeck attrs
      pure a
    _ -> TheMadmansNotebook <$> liftRunMessage msg attrs
