module Arkham.Location.Cards.TrophyRoom
  ( trophyRoom
  , TrophyRoom(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype TrophyRoom = TrophyRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trophyRoom :: LocationCard TrophyRoom
trophyRoom = location TrophyRoom Cards.trophyRoom 2 (Static 0)

instance HasAbilities TrophyRoom where
  getAbilities (TrophyRoom attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage TrophyRoom where
  runMessage msg (TrophyRoom attrs) =
    TrophyRoom <$> runMessage msg attrs
