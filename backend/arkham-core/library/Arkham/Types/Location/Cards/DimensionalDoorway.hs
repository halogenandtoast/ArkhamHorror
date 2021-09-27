module Arkham.Types.Location.Cards.DimensionalDoorway
  ( dimensionalDoorway
  , DimensionalDoorway(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (dimensionalDoorway)
import Arkham.Types.Ability
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EndTurn)
import Arkham.Types.Query
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Trait

newtype DimensionalDoorway = DimensionalDoorway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalDoorway :: LocationCard DimensionalDoorway
dimensionalDoorway = location
  DimensionalDoorway
  Cards.dimensionalDoorway
  2
  (PerPlayer 1)
  Squiggle
  [Triangle, Moon]

instance HasAbilities DimensionalDoorway where
  getAbilities (DimensionalDoorway attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility attrs 1 Here $ ForcedAbility $ TurnEnds
          Timing.When
          You
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env DimensionalDoorway where
  runMessage msg l@(DimensionalDoorway attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      encounterDiscard <- map unDiscardedEncounterCard <$> getList ()
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
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      resourceCount <- unResourceCount <$> getCount iid
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
