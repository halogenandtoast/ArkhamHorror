module Arkham.Location.Cards.AbbeyChurch
  ( abbeyChurch
  , AbbeyChurch(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message hiding (RevealLocation)
import Arkham.Timing qualified as Timing

newtype AbbeyChurch = AbbeyChurch LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abbeyChurch :: LocationCard AbbeyChurch
abbeyChurch = location
  AbbeyChurch
  Cards.abbeyChurch
  3
  (PerPlayer 1)
  Square
  [Equals, T, Heart, Hourglass, Moon]

instance HasAbilities AbbeyChurch where
  getAbilities (AbbeyChurch attrs) =
    withBaseAbilities attrs $ if locationRevealed attrs
      then
        [ mkAbility attrs 1
          $ ForcedAbility
          $ RevealLocation Timing.After Anyone
          $ LocationWithId
          $ toId attrs
        ]
      else []

instance RunMessage AbbeyChurch where
  runMessage msg l@(AbbeyChurch attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      let
        titles =
          [ "Chœur Gothique"
          , "Knight's Hall"
          , "Cloister"
          , "Chapel of St. Aubert"
          , "Abbey Tower"
          ]
      pushAll $ map (PlaceLocationMatching . CardWithTitle) titles
      pure l
    _ -> AbbeyChurch <$> runMessage msg attrs
