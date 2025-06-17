module Arkham.Treachery.Cards.InexplicableColdTheMidwinterGala (inexplicableColdTheMidwinterGala) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Cards.InexplicableCold
import Arkham.Treachery.Import.Lifted

newtype InexplicableColdTheMidwinterGala = InexplicableColdTheMidwinterGala InexplicableCold
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor, HasAbilities)

inexplicableColdTheMidwinterGala :: TreacheryCard InexplicableColdTheMidwinterGala
inexplicableColdTheMidwinterGala =
  treachery
    (InexplicableColdTheMidwinterGala . InexplicableCold)
    Cards.inexplicableColdTheMidwinterGala

instance RunMessage InexplicableColdTheMidwinterGala where
  runMessage msg (InexplicableColdTheMidwinterGala inner) =
    InexplicableColdTheMidwinterGala <$> runMessage msg inner
