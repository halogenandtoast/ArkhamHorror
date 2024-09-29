module Arkham.Agenda.Cards.SecretsOfTheSeaV1
  ( SecretsOfTheSeaV1(..)
  , secretsOfTheSeaV1
  ) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype SecretsOfTheSeaV1 = SecretsOfTheSeaV1 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretsOfTheSeaV1 :: AgendaCard SecretsOfTheSeaV1
secretsOfTheSeaV1 = agenda (1, A) SecretsOfTheSeaV1 Cards.secretsOfTheSeaV1 (Static 9)

instance RunMessage SecretsOfTheSeaV1 where
  runMessage msg a@(SecretsOfTheSeaV1 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> SecretsOfTheSeaV1 <$> liftRunMessage msg attrs
