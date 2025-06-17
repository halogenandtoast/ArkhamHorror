module Arkham.Agenda.Cards.AKillerParty (aKillerParty) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (modifySelect)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait

newtype AKillerParty = AKillerParty AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aKillerParty :: AgendaCard AKillerParty
aKillerParty = agenda (3, A) AKillerParty Cards.aKillerParty (Static 6)

instance HasModifiersFor AKillerParty where
  getModifiersFor (AKillerParty a) =
    when (onSide A a) $ modifySelect a Anyone [CannotParleyWith $ EnemyWithTrait LanternClub]

instance RunMessage AKillerParty where
  runMessage msg (AKillerParty attrs) = runQueueT $ case msg of
    -- TODO handle spellbound forced ability and agenda advancement
    _ -> AKillerParty <$> liftRunMessage msg attrs
