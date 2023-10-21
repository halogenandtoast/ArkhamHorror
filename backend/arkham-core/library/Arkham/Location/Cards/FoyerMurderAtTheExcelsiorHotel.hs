module Arkham.Location.Cards.FoyerMurderAtTheExcelsiorHotel
  ( foyerMurderAtTheExcelsiorHotel
  , FoyerMurderAtTheExcelsiorHotel(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype FoyerMurderAtTheExcelsiorHotel = FoyerMurderAtTheExcelsiorHotel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foyerMurderAtTheExcelsiorHotel :: LocationCard FoyerMurderAtTheExcelsiorHotel
foyerMurderAtTheExcelsiorHotel = location FoyerMurderAtTheExcelsiorHotel Cards.foyerMurderAtTheExcelsiorHotel 2 (PerPlayer 1)

instance HasAbilities FoyerMurderAtTheExcelsiorHotel where
  getAbilities (FoyerMurderAtTheExcelsiorHotel attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage FoyerMurderAtTheExcelsiorHotel where
  runMessage msg (FoyerMurderAtTheExcelsiorHotel attrs) =
    FoyerMurderAtTheExcelsiorHotel <$> runMessage msg attrs
