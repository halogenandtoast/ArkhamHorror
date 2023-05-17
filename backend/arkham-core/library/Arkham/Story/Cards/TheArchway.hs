module Arkham.Story.Cards.TheArchway (
  TheArchway (..),
  theArchway,
) where

import Arkham.Prelude

import Arkham.Helpers.Query
import Arkham.Helpers.SkillTest
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype TheArchway = TheArchway StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theArchway :: StoryCard TheArchway
theArchway = story TheArchway Cards.theArchway

instance RunMessage TheArchway where
  runMessage msg s@(TheArchway attrs) = case msg of
    ResolveStory iid story' | story' == toId attrs -> do
      setAsideDimStreets <-
        getSetAsideCardsMatching $
          CardWithTitle "Dim Streets"
      otherDimStreets <- case nonEmpty setAsideDimStreets of
        Nothing -> error "missing"
        Just xs -> sample xs
      dimStreets <- selectJust $ locationIs Locations.dimStreetsTheArchway
      pushAll
        [ beginSkillTest
            iid
            (toSource attrs)
            (InvestigatorTarget iid)
            SkillIntellect
            3
        , ReplaceLocation dimStreets otherDimStreets DefaultReplace
        ]
      pure s
    _ -> TheArchway <$> runMessage msg attrs
