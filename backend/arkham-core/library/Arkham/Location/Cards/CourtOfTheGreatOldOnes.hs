module Arkham.Location.Cards.CourtOfTheGreatOldOnes
  ( courtOfTheGreatOldOnes
  , CourtOfTheGreatOldOnes(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Message qualified as Msg

newtype CourtOfTheGreatOldOnes = CourtOfTheGreatOldOnes LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

courtOfTheGreatOldOnes :: LocationCard CourtOfTheGreatOldOnes
courtOfTheGreatOldOnes =
  location CourtOfTheGreatOldOnes Cards.courtOfTheGreatOldOnes 3 (PerPlayer 2)

instance HasAbilities CourtOfTheGreatOldOnes where
  getAbilities (CourtOfTheGreatOldOnes attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage CourtOfTheGreatOldOnes where
  runMessage msg (CourtOfTheGreatOldOnes attrs) = case msg of
    Msg.RevealLocation _ lid | lid == toId attrs -> do
      CourtOfTheGreatOldOnes <$> runMessage msg (attrs & labelL .~ "courtOfTheGreatOldOnes")
    _ -> CourtOfTheGreatOldOnes <$> runMessage msg attrs
