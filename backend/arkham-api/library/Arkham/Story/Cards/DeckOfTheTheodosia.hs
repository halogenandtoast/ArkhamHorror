module Arkham.Story.Cards.DeckOfTheTheodosia (deckOfTheTheodosia) where

import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype DeckOfTheTheodosia = DeckOfTheTheodosia StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deckOfTheTheodosia :: StoryCard DeckOfTheTheodosia
deckOfTheTheodosia = story DeckOfTheTheodosia Cards.deckOfTheTheodosia

instance RunMessage DeckOfTheTheodosia where
  runMessage msg s@(DeckOfTheTheodosia attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      mCoastalWaters <- getSetAsideCardMaybe Locations.coastalWaters
      mHedgeMaze <- getSetAsideCardMaybe Locations.hedgeMaze
      mStandingStones <- getSetAsideCardMaybe Locations.standingStones

      chooseOneM iid do
        for_ mCoastalWaters \loc ->
          withI18n $ keyVar "name" "Coastal Waters" $ labeled' "putSetAsideLocationIntoPlay" $ placeLocation_ loc
        for_ mHedgeMaze \loc ->
          withI18n $ keyVar "name" "Hedge Maze" $ labeled' "putSetAsideLocationIntoPlay" $ placeLocation_ loc
        for_ mStandingStones \loc ->
          withI18n $ keyVar "name" "Standing Stones" $ labeled' "putSetAsideLocationIntoPlay" $ placeLocation_ loc
      pure s
    _ -> DeckOfTheTheodosia <$> liftRunMessage msg attrs
