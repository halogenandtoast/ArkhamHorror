module Arkham.Agenda.Cards.WhereTheresSmoke (whereTheresSmoke) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies

newtype WhereTheresSmoke = WhereTheresSmoke AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whereTheresSmoke :: AgendaCard WhereTheresSmoke
whereTheresSmoke = agenda (0, A) WhereTheresSmoke Cards.whereTheresSmoke (Static 2)

instance RunMessage WhereTheresSmoke where
  runMessage msg a@(WhereTheresSmoke attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      spawnEnemy_ =<< genCard Enemies.theConductorBeastFromBeyondTheGate
      advanceAgendaDeck attrs
      pure a
    _ -> WhereTheresSmoke <$> liftRunMessage msg attrs
