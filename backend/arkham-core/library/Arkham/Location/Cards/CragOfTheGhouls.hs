module Arkham.Location.Cards.CragOfTheGhouls
  ( cragOfTheGhouls
  , CragOfTheGhouls(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype CragOfTheGhouls = CragOfTheGhouls LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cragOfTheGhouls :: LocationCard CragOfTheGhouls
cragOfTheGhouls = location CragOfTheGhouls Cards.cragOfTheGhouls 3 (PerPlayer 2)

instance HasAbilities CragOfTheGhouls where
  getAbilities (CragOfTheGhouls attrs) =
    extendRevealed attrs []

instance RunMessage CragOfTheGhouls where
  runMessage msg (CragOfTheGhouls attrs) =
    CragOfTheGhouls <$> runMessage msg attrs
