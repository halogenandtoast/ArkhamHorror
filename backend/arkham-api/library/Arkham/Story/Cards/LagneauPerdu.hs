module Arkham.Story.Cards.LagneauPerdu (lagneauPerdu) where

import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype LagneauPerdu = LagneauPerdu StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lagneauPerdu :: StoryCard LagneauPerdu
lagneauPerdu = story LagneauPerdu Cards.lagneauPerdu

instance RunMessage LagneauPerdu where
  runMessage msg s@(LagneauPerdu attrs) = runQueueT $ case msg of
    ResolveStory _ _ (is attrs -> True) -> do
      remember InterviewedJordan
      pure s
    _ -> LagneauPerdu <$> liftRunMessage msg attrs
