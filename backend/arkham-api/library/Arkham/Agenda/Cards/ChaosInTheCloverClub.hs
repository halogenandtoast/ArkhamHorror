module Arkham.Agenda.Cards.ChaosInTheCloverClub (chaosInTheCloverClub) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Matcher
import Arkham.Trait

newtype ChaosInTheCloverClub = ChaosInTheCloverClub AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chaosInTheCloverClub :: AgendaCard ChaosInTheCloverClub
chaosInTheCloverClub = agenda (3, A) ChaosInTheCloverClub Cards.chaosInTheCloverClub (Static 7)

instance HasAbilities ChaosInTheCloverClub where
  getAbilities (ChaosInTheCloverClub x) = [mkAbility x 1 $ forced $ PhaseBegins #when #enemy | onSide A x]

instance RunMessage ChaosInTheCloverClub where
  runMessage msg a@(ChaosInTheCloverClub attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      discardEach attrs $ EnemyWithTrait Criminal <> at_ (LocationWithEnemy $ withTrait Abomination)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R4
      pure a
    _ -> ChaosInTheCloverClub <$> liftRunMessage msg attrs
