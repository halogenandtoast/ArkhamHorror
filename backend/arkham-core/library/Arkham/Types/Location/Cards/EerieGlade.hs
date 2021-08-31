module Arkham.Types.Location.Cards.EerieGlade
  ( eerieGlade
  , EerieGlade(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (eerieGlade)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import Arkham.Types.Query
import qualified Arkham.Types.Timing as Timing

newtype EerieGlade = EerieGlade LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eerieGlade :: LocationCard EerieGlade
eerieGlade = locationWith
  EerieGlade
  Cards.eerieGlade
  4
  (PerPlayer 1)
  NoSymbol
  []
  ((revealedSymbolL .~ Hourglass)
  . (revealedConnectedSymbolsL .~ setFromList [Triangle, Plus])
  )

instance HasAbilities env EerieGlade where
  getAbilities iid window (EerieGlade attrs) =
    withBaseAbilities iid window attrs $ pure
      [ restrictedAbility
          attrs
          1
          (InvestigatorExists $ You <> InvestigatorWithAnyActionsRemaining)
        $ ForcedAbility
        $ RevealLocation Timing.After You
        $ LocationWithId
        $ toId attrs
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env EerieGlade where
  runMessage msg l@(EerieGlade attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      actionRemainingCount <- unActionRemainingCount <$> getCount iid
      l <$ push (DiscardTopOfDeck iid (actionRemainingCount * 2) Nothing)
    _ -> EerieGlade <$> runMessage msg attrs
