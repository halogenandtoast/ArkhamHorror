module Arkham.Story.Cards.TracesToNowhere.AGlimpseBeyond (aGlimpseBeyond) where

import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Story.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Story.Import.Lifted

newtype AGlimpseBeyond = AGlimpseBeyond StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aGlimpseBeyond :: StoryCard AGlimpseBeyond
aGlimpseBeyond = story AGlimpseBeyond Cards.aGlimpseBeyond

instance RunMessage AGlimpseBeyond where
  runMessage msg s@(AGlimpseBeyond attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      remember TheInvestigatorsFoundSomethingStrangeInTheFuneralHomeBasement
      pure s
    _ -> AGlimpseBeyond <$> liftRunMessage msg attrs
