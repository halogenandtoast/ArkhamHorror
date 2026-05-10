module Arkham.Story.Cards.PrisonOfMemories (prisonOfMemories) where

import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype PrisonOfMemories = PrisonOfMemories StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prisonOfMemories :: StoryCard PrisonOfMemories
prisonOfMemories = story PrisonOfMemories Cards.prisonOfMemories

instance RunMessage PrisonOfMemories where
  runMessage msg s@(PrisonOfMemories attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      mBaseCamp <- getSetAsideCardMaybe Locations.baseCamp
      mDeckOfTheTheodosia <- getSetAsideCardMaybe Locations.deckOfTheTheodosia
      mUniversityOfHalls <- getSetAsideCardMaybe Locations.universityHalls

      chooseOneM iid do
        for_ mBaseCamp \loc ->
          withI18n $ keyVar "name" "Base Camp" $ labeled' "putSetAsideLocationIntoPlay" $ placeLocation_ loc
        for_ mDeckOfTheTheodosia \loc ->
          withI18n $ keyVar "name" "Deck of the Theodosia" $ labeled' "putSetAsideLocationIntoPlay" $ placeLocation_ loc
        for_ mUniversityOfHalls \loc ->
          withI18n $ keyVar "name" "University Halls" $ labeled' "putSetAsideLocationIntoPlay" $ placeLocation_ loc

      pure s
    _ -> PrisonOfMemories <$> liftRunMessage msg attrs
