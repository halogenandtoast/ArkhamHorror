module Arkham.Story.Cards.InhabitantOfCarcosa (inhabitantOfCarcosa) where

import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (DefaultReplace))
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype InhabitantOfCarcosa = InhabitantOfCarcosa StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inhabitantOfCarcosa :: StoryCard InhabitantOfCarcosa
inhabitantOfCarcosa = story InhabitantOfCarcosa Cards.inhabitantOfCarcosa

instance RunMessage InhabitantOfCarcosa where
  runMessage msg s@(InhabitantOfCarcosa attrs) = runQueueT $ case msg of
    ResolveStory _ _ story' | story' == toId attrs -> do
      ruinsOfCarcosa <- selectJust $ locationIs Locations.ruinsOfCarcosaInhabitantOfCarcosa
      setAsideRuinsOfCarcosa <- getSetAsideCardsMatching $ CardWithTitle "Ruins of Carcosa"
      otherRuinsOfCarcosa <- case nonEmpty setAsideRuinsOfCarcosa of
        Nothing -> error "missing"
        Just xs -> sample xs
      selectEach (HealableInvestigator (toSource attrs) #horror Anyone) \iid -> healHorror iid attrs 3
      push $ ReplaceLocation ruinsOfCarcosa otherRuinsOfCarcosa DefaultReplace
      pure s
    _ -> InhabitantOfCarcosa <$> liftRunMessage msg attrs
