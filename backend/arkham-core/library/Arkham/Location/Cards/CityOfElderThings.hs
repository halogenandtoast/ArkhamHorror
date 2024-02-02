module Arkham.Location.Cards.CityOfElderThings (
  cityOfElderThings,
  CityOfElderThings (..),
) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Scenario.Deck
import Arkham.Timing qualified as Timing

newtype CityOfElderThings = CityOfElderThings LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

cityOfElderThings :: LocationCard CityOfElderThings
cityOfElderThings =
  location CityOfElderThings Cards.cityOfElderThings 3 (PerPlayer 2)

instance HasAbilities CityOfElderThings where
  getAbilities (CityOfElderThings a) =
    withRevealedAbilities
      a
      [ mkAbility a 1
          $ ReactionAbility
            (RevealLocation Timing.After You $ LocationWithId $ toId a)
            (HorrorCost (toSource a) YouTarget 2)
      ]

instance RunMessage CityOfElderThings where
  runMessage msg l@(CityOfElderThings attrs) = case msg of
    Msg.RevealLocation _ lid | lid == toId attrs -> do
      CityOfElderThings <$> runMessage msg (attrs & labelL .~ "cityOfElderThings")
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ DrawFromScenarioDeck iid UnknownPlacesDeck (toTarget attrs) 1
      pure l
    DrewFromScenarioDeck _ _ (isTarget attrs -> True) [card] -> do
      labels <-
        selectFields LocationLabel
          $ LocationWithUnrevealedTitle "Unknown Places"
          <> NotLocation RevealedLocation
      let
        nextLabel =
          fromJustNote "too many locations"
            $ find (`notElem` labels)
            $ map
              (\n -> "unknownPlaces" <> tshow n)
              ([1 .. 7] :: [Int])
      (lid, placement) <- placeLocation card
      pushAll
        [ placement
        , AddDirectConnection (toId attrs) lid
        , AddDirectConnection lid (toId attrs)
        , SetLocationLabel lid nextLabel
        ]
      pure l
    _ -> CityOfElderThings <$> runMessage msg attrs
