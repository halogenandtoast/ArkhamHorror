module Arkham.Agenda.Cards.SecretsOfTheSeaV2
  ( SecretsOfTheSeaV2(..)
  , secretsOfTheSeaV2
  ) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype SecretsOfTheSeaV2 = SecretsOfTheSeaV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretsOfTheSeaV2 :: AgendaCard SecretsOfTheSeaV2
secretsOfTheSeaV2 = agenda (1, A) SecretsOfTheSeaV2 Cards.secretsOfTheSeaV2 (Static 12)

instance RunMessage SecretsOfTheSeaV2 where
  runMessage msg a@(SecretsOfTheSeaV2 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> SecretsOfTheSeaV2 <$> liftRunMessage msg attrs
