module Arkham.Story.Cards.TheKingsParade (TheKingsParade (..), theKingsParade) where

import Arkham.Helpers.Query
import Arkham.Helpers.SkillTest
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype TheKingsParade = TheKingsParade StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKingsParade :: StoryCard TheKingsParade
theKingsParade = story TheKingsParade Cards.theKingsParade

instance RunMessage TheKingsParade where
  runMessage msg s@(TheKingsParade attrs) = case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      setAsideDimStreets <- getSetAsideCardsMatching $ CardWithTitle "Dim Streets"
      otherDimStreets <- case nonEmpty setAsideDimStreets of
        Nothing -> error "missing"
        Just xs -> sample xs
      dimStreets <- selectJust $ locationIs Locations.dimStreetsTheKingsParade
      sid <- getRandom
      pushAll
        [ beginSkillTest sid iid attrs iid #combat (Fixed 2)
        , ReplaceLocation dimStreets otherDimStreets DefaultReplace
        ]
      pure s
    _ -> TheKingsParade <$> runMessage msg attrs
