module Arkham.Story.Cards.InhabitantOfCarcosa (
  InhabitantOfCarcosa (..),
  inhabitantOfCarcosa,
) where

import Arkham.Prelude

import Arkham.Helpers.Investigator
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype InhabitantOfCarcosa = InhabitantOfCarcosa StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inhabitantOfCarcosa :: StoryCard InhabitantOfCarcosa
inhabitantOfCarcosa = story InhabitantOfCarcosa Cards.inhabitantOfCarcosa

instance RunMessage InhabitantOfCarcosa where
  runMessage msg s@(InhabitantOfCarcosa attrs) = case msg of
    ResolveStory _ _ story' | story' == toId attrs -> do
      healHorrorMessages <-
        map snd <$> getInvestigatorsWithHealHorror attrs 3 Anyone
      ruinsOfCarcosa <- selectJust $ locationIs Locations.ruinsOfCarcosaInhabitantOfCarcosa
      setAsideRuinsOfCarcosa <-
        getSetAsideCardsMatching
          $ CardWithTitle "Ruins of Carcosa"
      otherRuinsOfCarcosa <- case nonEmpty setAsideRuinsOfCarcosa of
        Nothing -> error "missing"
        Just xs -> sample xs
      pushAll
        $ healHorrorMessages
        <> [ReplaceLocation ruinsOfCarcosa otherRuinsOfCarcosa DefaultReplace]
      pure s
    _ -> InhabitantOfCarcosa <$> runMessage msg attrs
