module Arkham.Story.Cards.TheHeightOfTheDepths (theHeightOfTheDepths) where

import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (DefaultReplace))
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheHeightOfTheDepths = TheHeightOfTheDepths StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHeightOfTheDepths :: StoryCard TheHeightOfTheDepths
theHeightOfTheDepths = story TheHeightOfTheDepths Cards.theHeightOfTheDepths

instance RunMessage TheHeightOfTheDepths where
  runMessage msg s@(TheHeightOfTheDepths attrs) = runQueueT $ case msg of
    ResolveStory _ _ (is attrs -> True) -> do
      selectEach (HealableInvestigator (toSource attrs) #horror Anyone) \iid -> healHorror iid attrs 5
      setAsideDepthsOfDemhe <- getSetAsideCardsMatching $ CardWithTitle "Depths of Demhe"
      otherDepthsOfDemhe <- case nonEmpty setAsideDepthsOfDemhe of
        Nothing -> error "missing"
        Just xs -> sample xs
      depthsOfDemhe <- selectJust $ locationIs Locations.depthsOfDemheTheHeightOfTheDepths
      push $ ReplaceLocation depthsOfDemhe otherDepthsOfDemhe DefaultReplace
      pure s
    _ -> TheHeightOfTheDepths <$> liftRunMessage msg attrs
