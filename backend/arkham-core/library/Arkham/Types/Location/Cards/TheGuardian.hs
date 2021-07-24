module Arkham.Types.Location.Cards.TheGuardian
  ( theGuardian
  , TheGuardian(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Window

newtype TheGuardian = TheGuardian LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGuardian :: LocationCard TheGuardian
theGuardian = locationWith
  TheGuardian
  Cards.theGuardian
  3
  (PerPlayer 2)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

instance HasModifiersFor env TheGuardian

ability :: LocationAttrs -> Ability
ability attrs = mkAbility attrs 1 (ReactionAbility Free)

instance ActionRunner env => HasActions env TheGuardian where
  getActions iid (AfterEntering You lid) (TheGuardian attrs)
    | lid == toId attrs = pure [UseAbility iid (ability attrs)]
  getActions iid window (TheGuardian attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env TheGuardian where
  runMessage msg l@(TheGuardian attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (DrawCards iid 1 False)
    _ -> TheGuardian <$> runMessage msg attrs
