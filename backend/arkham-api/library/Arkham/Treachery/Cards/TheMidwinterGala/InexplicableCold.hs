module Arkham.Treachery.Cards.TheMidwinterGala.InexplicableCold (inexplicableCold) where

import Arkham.Treachery.CardDefs.TheMidwinterGala qualified as Cards
import Arkham.Treachery.Cards.ReturnToTheDunwichLegacy.CreepingCold.InexplicableCold qualified as Base
import Arkham.Treachery.Import.Lifted

newtype InexplicableCold = InexplicableCold Base.InexplicableCold
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor, HasAbilities)

inexplicableCold :: TreacheryCard InexplicableCold
inexplicableCold =
  treachery
    (InexplicableCold . Base.InexplicableCold)
    Cards.inexplicableCold

instance RunMessage InexplicableCold where
  runMessage msg (InexplicableCold inner) =
    InexplicableCold <$> runMessage msg inner
