module Arkham.Location.Cards.DimensionalDoorway
  ( dimensionalDoorway
  , DimensionalDoorway(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Cards ( dimensionalDoorway )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message hiding ( EndTurn )
import Arkham.Projection
import Arkham.Scenario.Types ( Field (..) )
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype DimensionalDoorway = DimensionalDoorway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalDoorway :: LocationCard DimensionalDoorway
dimensionalDoorway =
  location DimensionalDoorway Cards.dimensionalDoorway 2 (PerPlayer 1)

instance HasAbilities DimensionalDoorway where
  getAbilities (DimensionalDoorway attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility attrs 1 Here $ ForcedAbility $ TurnEnds
            Timing.When
            You
        | locationRevealed attrs
        ]

instance RunMessage DimensionalDoorway where
  runMessage msg l@(DimensionalDoorway attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      encounterDiscard <- scenarioField ScenarioDiscard
      let
        mHexCard = find (member Hex . toTraits) encounterDiscard
        revelationMsgs = case mHexCard of
          Nothing -> []
          Just hexCard ->
            [ RemoveFromEncounterDiscard hexCard
            , InvestigatorDrewEncounterCard iid hexCard
            ]
      pushAll revelationMsgs
      DimensionalDoorway <$> runMessage msg attrs
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      resourceCount <- getSpendableResources iid
      if resourceCount >= 2
        then l <$ push
          (chooseOne
            iid
            [ Label "Spend 2 resource" [SpendResources iid 2]
            , Label
              "Shuffle Dimensional Doorway back into the encounter deck"
              [ShuffleBackIntoEncounterDeck $ toTarget attrs]
            ]
          )
        else l <$ push (ShuffleBackIntoEncounterDeck $ toTarget attrs)
    _ -> DimensionalDoorway <$> runMessage msg attrs
