module Arkham.Types.Location.Cards.EerieGlade
  ( eerieGlade
  , EerieGlade(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (eerieGlade)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Message hiding (RevealLocation)
import Arkham.Types.Query
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

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

forcedAbility :: LocationAttrs -> Ability
forcedAbility a = mkAbility (toSource a) 1 LegacyForcedAbility

instance ActionRunner env => HasAbilities env EerieGlade where
  getAbilities iid (Window Timing.After (RevealLocation who _)) (EerieGlade attrs)
    | iid == who
    = do
      actionRemainingCount <- unActionRemainingCount <$> getCount iid
      pure [ locationAbility (forcedAbility attrs) | actionRemainingCount > 0 ]
  getAbilities iid window (EerieGlade attrs) = getAbilities iid window attrs

instance LocationRunner env => RunMessage env EerieGlade where
  runMessage msg l@(EerieGlade attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      actionRemainingCount <- unActionRemainingCount <$> getCount iid
      l <$ push (DiscardTopOfDeck iid (actionRemainingCount * 2) Nothing)
    _ -> EerieGlade <$> runMessage msg attrs
