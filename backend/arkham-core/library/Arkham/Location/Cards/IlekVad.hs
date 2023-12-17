module Arkham.Location.Cards.IlekVad
  ( ilekVad
  , IlekVad(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype IlekVad = IlekVad LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ilekVad :: LocationCard IlekVad
ilekVad = location IlekVad Cards.ilekVad 2 (PerPlayer 1)

instance HasAbilities IlekVad where
  getAbilities (IlekVad attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage IlekVad where
  runMessage msg (IlekVad attrs) =
    IlekVad <$> runMessage msg attrs
