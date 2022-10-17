module Arkham.Location.Cards.VastPassages
  ( vastPassages
  , VastPassages(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype VastPassages = VastPassages LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vastPassages :: LocationCard VastPassages
vastPassages = location VastPassages Cards.vastPassages 2 (PerPlayer 1)

instance HasAbilities VastPassages where
  getAbilities (VastPassages attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage VastPassages where
  runMessage msg (VastPassages attrs) =
    VastPassages <$> runMessage msg attrs
