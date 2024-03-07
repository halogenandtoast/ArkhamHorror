module Arkham.Location.Cards.UpstairsDoorwayLibrary
  ( upstairsDoorwayLibrary
  , UpstairsDoorwayLibrary(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype UpstairsDoorwayLibrary = UpstairsDoorwayLibrary LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

upstairsDoorwayLibrary :: LocationCard UpstairsDoorwayLibrary
upstairsDoorwayLibrary = location UpstairsDoorwayLibrary Cards.upstairsDoorwayLibrary 0 (Static 0)

instance HasAbilities UpstairsDoorwayLibrary where
  getAbilities (UpstairsDoorwayLibrary attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage UpstairsDoorwayLibrary where
  runMessage msg (UpstairsDoorwayLibrary attrs) =
    UpstairsDoorwayLibrary <$> runMessage msg attrs
