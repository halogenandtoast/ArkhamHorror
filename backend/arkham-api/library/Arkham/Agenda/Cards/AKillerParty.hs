module Arkham.Agenda.Cards.AKillerParty (aKillerParty) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Helpers.Window (assetLeavingPlay)
import Arkham.Matcher
import Arkham.Scenarios.TheMidwinterGala.Helpers (becomeSpellbound)
import Arkham.Trait

newtype AKillerParty = AKillerParty AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aKillerParty :: AgendaCard AKillerParty
aKillerParty = agenda (3, A) AKillerParty Cards.aKillerParty (Static 6)

instance HasModifiersFor AKillerParty where
  getModifiersFor (AKillerParty a) = do
    when (onSide A a) $ modifySelect a Anyone [CannotParleyWith $ EnemyWithTrait LanternClub]
    modifySelf a [CannotRemoveDoomOnThis]

instance RunMessage AKillerParty where
  runMessage msg a@(AKillerParty attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (assetLeavingPlay -> aid) _ -> do
      becomeSpellbound aid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferMentalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> AKillerParty <$> liftRunMessage msg attrs
