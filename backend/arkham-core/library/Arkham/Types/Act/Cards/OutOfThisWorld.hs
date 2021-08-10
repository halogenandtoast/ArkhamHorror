module Arkham.Types.Act.Cards.OutOfThisWorld
  ( OutOfThisWorld(..)
  , outOfThisWorld
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Location.Cards as Locations
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Target

newtype OutOfThisWorld = OutOfThisWorld ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

outOfThisWorld :: ActCard OutOfThisWorld
outOfThisWorld = act
  (1, A)
  OutOfThisWorld
  Cards.outOfThisWorld
  (Just $ GroupClueCost (PerPlayer 2) Nothing)

instance HasActions OutOfThisWorld where
  getActions (OutOfThisWorld x) =
    mkAbility x 1 (ActionAbility Nothing $ ActionCost 1) : getActions x

instance ActRunner env => RunMessage env OutOfThisWorld where
  runMessage msg a@(OutOfThisWorld attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      theEdgeOfTheUniverseId <- getRandom
      a <$ pushAll
        [ PlaceLocation theEdgeOfTheUniverseId Locations.theEdgeOfTheUniverse
        , NextAct actId "02317"
        ]
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DiscardTopOfEncounterDeck iid 3 (Just $ toTarget attrs))
    DiscardedTopOfEncounterDeck iid cards target | isTarget attrs target -> do
      let locationCards = filterLocations cards
      a <$ unless
        (null locationCards)
        (pushAll
          [ FocusCards (map EncounterCard locationCards)
          , chooseOne
            iid
            [ TargetLabel
                (CardIdTarget $ toCardId location)
                [ RemoveFromEncounterDiscard location
                , InvestigatorDrewEncounterCard iid location
                ]
            | location <- locationCards
            ]
          , UnfocusCards
          ]
        )
    _ -> OutOfThisWorld <$> runMessage msg attrs
