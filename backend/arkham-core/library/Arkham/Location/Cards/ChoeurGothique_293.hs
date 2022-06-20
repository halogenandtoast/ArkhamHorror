module Arkham.Location.Cards.ChoeurGothique_293
  ( choeurGothique_293
  , ChoeurGothique_293(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype ChoeurGothique_293 = ChoeurGothique_293 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

choeurGothique_293 :: LocationCard ChoeurGothique_293
choeurGothique_293 = location ChoeurGothique_293 Cards.choeurGothique_293 3 (PerPlayer 1) T [Square, Star]

instance HasAbilities ChoeurGothique_293 where
  getAbilities (ChoeurGothique_293 attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage ChoeurGothique_293 where
  runMessage msg (ChoeurGothique_293 attrs) =
    ChoeurGothique_293 <$> runMessage msg attrs
