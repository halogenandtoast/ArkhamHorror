module Arkham.Location.Cards.UprootedWoods
  ( uprootedWoods
  , UprootedWoods(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (uprootedWoods)
import Arkham.Classes
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Matcher
import Arkham.Message hiding (RevealLocation)
import Arkham.Timing qualified as Timing

newtype UprootedWoods = UprootedWoods LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uprootedWoods :: LocationCard UprootedWoods
uprootedWoods = locationWithRevealedSideConnections
  UprootedWoods
  Cards.uprootedWoods
  2
  (PerPlayer 1)
  NoSymbol
  []
  Moon
  [Square, T]

instance HasAbilities UprootedWoods where
  getAbilities (UprootedWoods attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility
            attrs
            1
            (InvestigatorExists $ You <> InvestigatorWithoutActionsRemaining)
          $ ForcedAbility
          $ RevealLocation Timing.After You
          $ LocationWithId
          $ toId attrs
        | locationRevealed attrs
        ]

instance LocationRunner env => RunMessage env UprootedWoods where
  runMessage msg l@(UprootedWoods attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      l <$ push (DiscardTopOfDeck iid 5 Nothing)
    _ -> UprootedWoods <$> runMessage msg attrs
