module Arkham.Act.Cards.CloseTheRift (
  CloseTheRift (..),
  closeTheRift,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype CloseTheRift = CloseTheRift ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

closeTheRift :: ActCard CloseTheRift
closeTheRift =
  act
    (3, A)
    CloseTheRift
    Cards.closeTheRift
    ( Just
        $ GroupClueCost
          (PerPlayer 3)
          (LocationWithTitle "The Edge of the Universe")
    )

instance HasAbilities CloseTheRift where
  getAbilities (CloseTheRift x) =
    withBaseAbilities x [mkAbility x 1 $ ActionAbility Nothing $ ActionCost 1]

instance RunMessage CloseTheRift where
  runMessage msg a@(CloseTheRift attrs@ActAttrs {..}) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ DiscardTopOfEncounterDeck iid 3 (toSource attrs) (Just $ toTarget attrs)
      pure a
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      theEdgeOfTheUniverseId <- getJustLocationByName "The Edge of the Universe"
      tearThroughTime <- getSetAsideCard Locations.tearThroughTime
      locationPlacement <- placeLocation_ tearThroughTime
      pushAll
        $ resolve (RemoveLocation theEdgeOfTheUniverseId)
        <> [locationPlacement, AdvanceActDeck actDeckId (toSource attrs)]
      pure a
    DiscardedTopOfEncounterDeck iid cards _ target | isTarget attrs target -> do
      let locationCards = filterLocations cards
      player <- getPlayer iid
      unless (null locationCards)
        $ pushAll
          [ FocusCards (map EncounterCard locationCards)
          , chooseOne
              player
              [ TargetLabel
                (CardIdTarget $ toCardId location)
                [InvestigatorDrewEncounterCard iid location]
              | location <- locationCards
              ]
          ]
      pure a
    _ -> CloseTheRift <$> runMessage msg attrs
