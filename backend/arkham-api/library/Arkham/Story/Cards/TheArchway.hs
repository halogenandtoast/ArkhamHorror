module Arkham.Story.Cards.TheArchway (theArchway) where

import Arkham.Helpers.Investigator
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (DefaultReplace))
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheArchway = TheArchway StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theArchway :: StoryCard TheArchway
theArchway = story TheArchway Cards.theArchway

instance RunMessage TheArchway where
  runMessage msg s@(TheArchway attrs) = runQueueT $ case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      setAsideDimStreets <- getSetAsideCardsMatching $ CardWithTitle "Dim Streets"
      otherDimStreets <- case nonEmpty setAsideDimStreets of
        Nothing -> error "missing"
        Just xs -> sample xs
      dimStreets <- selectJust $ locationIs Locations.dimStreetsTheArchway
      sid <- getRandom
      beginSkillTest sid iid attrs iid #willpower (Fixed 1)
      push $ ReplaceLocation dimStreets otherDimStreets DefaultReplace
      pure s
    PassedThisSkillTestBy iid (isSource attrs -> True) n -> do
      whenM (canHaveHorrorHealed attrs iid) $ healHorror iid attrs n
      pure s
    _ -> TheArchway <$> liftRunMessage msg attrs
