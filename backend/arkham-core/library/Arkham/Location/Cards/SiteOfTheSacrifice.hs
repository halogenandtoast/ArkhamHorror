module Arkham.Location.Cards.SiteOfTheSacrifice
  ( siteOfTheSacrifice
  , SiteOfTheSacrifice(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SiteOfTheSacrifice = SiteOfTheSacrifice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

siteOfTheSacrifice :: LocationCard SiteOfTheSacrifice
siteOfTheSacrifice =
  location SiteOfTheSacrifice Cards.siteOfTheSacrifice 4 (PerPlayer 3)

instance HasAbilities SiteOfTheSacrifice where
  getAbilities (SiteOfTheSacrifice attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage SiteOfTheSacrifice where
  runMessage msg (SiteOfTheSacrifice attrs) =
    SiteOfTheSacrifice <$> runMessage msg attrs
