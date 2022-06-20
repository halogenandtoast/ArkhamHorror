module Arkham.Location.Cards.ChoeurGothique_292
  ( choeurGothique_292
  , ChoeurGothique_292(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype ChoeurGothique_292 = ChoeurGothique_292 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

choeurGothique_292 :: LocationCard ChoeurGothique_292
choeurGothique_292 = location ChoeurGothique_292 Cards.choeurGothique_292 3 (PerPlayer 1) T [Square, Star]

instance HasAbilities ChoeurGothique_292 where
  getAbilities (ChoeurGothique_292 attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage ChoeurGothique_292 where
  runMessage msg (ChoeurGothique_292 attrs) =
    ChoeurGothique_292 <$> runMessage msg attrs
