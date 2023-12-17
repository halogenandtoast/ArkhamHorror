module Arkham.Location.Cards.NamelessRuins
  ( namelessRuins
  , NamelessRuins(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype NamelessRuins = NamelessRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

namelessRuins :: LocationCard NamelessRuins
namelessRuins = location NamelessRuins Cards.namelessRuins 5 (PerPlayer 1)

instance HasAbilities NamelessRuins where
  getAbilities (NamelessRuins attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage NamelessRuins where
  runMessage msg (NamelessRuins attrs) =
    NamelessRuins <$> runMessage msg attrs
