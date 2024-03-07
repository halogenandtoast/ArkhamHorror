module Arkham.Location.Cards.DownstairsDoorwayParlor
  ( downstairsDoorwayParlor
  , DownstairsDoorwayParlor(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype DownstairsDoorwayParlor = DownstairsDoorwayParlor LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downstairsDoorwayParlor :: LocationCard DownstairsDoorwayParlor
downstairsDoorwayParlor = location DownstairsDoorwayParlor Cards.downstairsDoorwayParlor 0 (Static 0)

instance HasAbilities DownstairsDoorwayParlor where
  getAbilities (DownstairsDoorwayParlor attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage DownstairsDoorwayParlor where
  runMessage msg (DownstairsDoorwayParlor attrs) =
    DownstairsDoorwayParlor <$> runMessage msg attrs
