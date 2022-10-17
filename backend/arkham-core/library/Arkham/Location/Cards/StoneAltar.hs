module Arkham.Location.Cards.StoneAltar
  ( stoneAltar
  , StoneAltar(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype StoneAltar = StoneAltar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stoneAltar :: LocationCard StoneAltar
stoneAltar = location StoneAltar Cards.stoneAltar 3 (PerPlayer 1)

instance HasAbilities StoneAltar where
  getAbilities (StoneAltar attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage StoneAltar where
  runMessage msg (StoneAltar attrs) =
    StoneAltar <$> runMessage msg attrs
