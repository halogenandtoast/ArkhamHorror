module Arkham.Story.Cards.HedgeMaze (hedgeMaze) where

import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype HedgeMaze = HedgeMaze StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hedgeMaze :: StoryCard HedgeMaze
hedgeMaze = story HedgeMaze Cards.hedgeMaze

instance RunMessage HedgeMaze where
  runMessage msg s@(HedgeMaze attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      mMoaiStatues <- getSetAsideCardMaybe Locations.moaiStatues

      chooseOneM iid do
        for_ mMoaiStatues \loc ->
          withI18n $ keyVar "name" "Mo'ai Statues" $ labeled' "putSetAsideLocationIntoPlay" $ placeLocation_ loc
      pure s
    _ -> HedgeMaze <$> liftRunMessage msg attrs
