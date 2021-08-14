module Arkham.Types.Location.Cards.FrozenSpring
  ( frozenSpring
  , FrozenSpring(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (frozenSpring)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Window

newtype FrozenSpring = FrozenSpring LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frozenSpring :: LocationCard FrozenSpring
frozenSpring = locationWith
  FrozenSpring
  Cards.frozenSpring
  3
  (PerPlayer 1)
  NoSymbol
  []
  ((revealedSymbolL .~ Plus)
  . (revealedConnectedSymbolsL .~ setFromList [Triangle, Hourglass])
  )

instance HasModifiersFor env FrozenSpring

forcedAbility :: LocationAttrs -> Ability
forcedAbility a = mkAbility (toSource a) 1 ForcedAbility

instance ActionRunner env => HasAbilities env FrozenSpring where
  getAbilities iid (AfterRevealLocation who) (FrozenSpring attrs) | iid == who =
    pure [locationAbility (forcedAbility attrs)]
  getAbilities iid window (FrozenSpring attrs) = getAbilities iid window attrs

instance LocationRunner env => RunMessage env FrozenSpring where
  runMessage msg l@(FrozenSpring attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      l <$ pushAll [SetActions iid (toSource attrs) 0, ChooseEndTurn iid]
    _ -> FrozenSpring <$> runMessage msg attrs
