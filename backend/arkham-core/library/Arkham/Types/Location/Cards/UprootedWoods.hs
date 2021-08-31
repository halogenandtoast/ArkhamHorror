module Arkham.Types.Location.Cards.UprootedWoods
  ( uprootedWoods
  , UprootedWoods(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (uprootedWoods)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import qualified Arkham.Types.Timing as Timing

newtype UprootedWoods = UprootedWoods LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uprootedWoods :: LocationCard UprootedWoods
uprootedWoods = locationWith
  UprootedWoods
  Cards.uprootedWoods
  2
  (PerPlayer 1)
  NoSymbol
  []
  ((revealedSymbolL .~ Moon)
  . (revealedConnectedSymbolsL .~ setFromList [Square, T])
  )

instance HasAbilities env UprootedWoods where
  getAbilities iid window (UprootedWoods attrs) =
    withBaseAbilities iid window attrs $ pure
      [ restrictedAbility
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
