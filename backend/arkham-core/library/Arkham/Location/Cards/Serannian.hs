module Arkham.Location.Cards.Serannian
  ( serannian
  , Serannian(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Serannian = Serannian LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

serannian :: LocationCard Serannian
serannian = location Serannian Cards.serannian 3 (PerPlayer 1)

instance HasAbilities Serannian where
  getAbilities (Serannian attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage Serannian where
  runMessage msg (Serannian attrs) =
    Serannian <$> runMessage msg attrs
