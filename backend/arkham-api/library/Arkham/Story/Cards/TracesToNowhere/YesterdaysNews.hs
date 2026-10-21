module Arkham.Story.Cards.TracesToNowhere.YesterdaysNews (yesterdaysNews) where

import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Story.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Story.Import.Lifted

newtype YesterdaysNews = YesterdaysNews StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yesterdaysNews :: StoryCard YesterdaysNews
yesterdaysNews = story YesterdaysNews Cards.yesterdaysNews

instance RunMessage YesterdaysNews where
  runMessage msg s@(YesterdaysNews attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      remember TheInvestigatorsFoundRecordOfMatildesBribe
      pure s
    _ -> YesterdaysNews <$> liftRunMessage msg attrs
