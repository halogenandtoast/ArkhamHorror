module Arkham.Types.Location.Cards.CanalSide
  ( canalSide
  , CanalSide(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

newtype CanalSide = CanalSide LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

canalSide :: LocationCard CanalSide
canalSide = locationWith
  CanalSide
  Cards.canalSide
  2
  (Static 1)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

ability :: LocationAttrs -> Ability
ability attrs = mkAbility attrs 1 (LegacyReactionAbility Free)

instance HasAbilities env CanalSide where
  getAbilities iid (Window Timing.After (Entering who lid)) (CanalSide attrs)
    | lid == toId attrs && iid == who = pure [ability attrs]
  getAbilities iid window (CanalSide attrs) = getAbilities iid window attrs

instance LocationRunner env => RunMessage env CanalSide where
  runMessage msg l@(CanalSide attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      l <$ push (PlaceClues (toTarget attrs) 1)
    _ -> CanalSide <$> runMessage msg attrs
