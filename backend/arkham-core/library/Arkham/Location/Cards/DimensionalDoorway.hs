module Arkham.Location.Cards.DimensionalDoorway
  ( dimensionalDoorway
  , DimensionalDoorway(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( dimensionalDoorway )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
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
      for_ (find (member Hex . toTraits) encounterDiscard)
        $ \hexCard -> push $ InvestigatorDrewEncounterCard iid hexCard
      DimensionalDoorway <$> runMessage msg attrs
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      resourceCount <- getSpendableResources iid
      push $ if resourceCount >= 2
        then chooseOne
          iid
          [ Label "Spend 2 resource" [SpendResources iid 2]
          , Label
            "Shuffle Dimensional Doorway back into the encounter deck"
            [ShuffleBackIntoEncounterDeck $ toTarget attrs]
          ]
        else ShuffleBackIntoEncounterDeck (toTarget attrs)
      pure l
    _ -> DimensionalDoorway <$> runMessage msg attrs
