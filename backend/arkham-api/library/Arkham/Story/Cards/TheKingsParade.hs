module Arkham.Story.Cards.TheKingsParade (theKingsParade) where

import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (DefaultReplace))
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheKingsParade = TheKingsParade StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKingsParade :: StoryCard TheKingsParade
theKingsParade = story TheKingsParade Cards.theKingsParade

instance RunMessage TheKingsParade where
  runMessage msg s@(TheKingsParade attrs) = runQueueT $ case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      setAsideDimStreets <- getSetAsideCardsMatching $ CardWithTitle "Dim Streets"
      otherDimStreets <- case nonEmpty setAsideDimStreets of
        Nothing -> error "missing"
        Just xs -> sample xs
      dimStreets <- selectJust $ locationIs Locations.dimStreetsTheKingsParade
      sid <- getRandom
      beginSkillTest sid iid attrs iid #combat (Fixed 2)
      push $ ReplaceLocation dimStreets otherDimStreets DefaultReplace
      pure s
    PassedThisSkillTest _ (isSource attrs -> True) -> do
      hastur <- selectJust $ EnemyWithTitle "Hastur"
      exhaustThis hastur
      selectEach (investigatorEngagedWith hastur) (`disengageEnemy` hastur)
      pure s
    _ -> TheKingsParade <$> liftRunMessage msg attrs
