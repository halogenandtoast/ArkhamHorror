module Arkham.Agenda.Cards.UnexpectedGuests (unexpectedGuests) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (modifySelect)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait

newtype UnexpectedGuests = UnexpectedGuests AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unexpectedGuests :: AgendaCard UnexpectedGuests
unexpectedGuests = agenda (2, A) UnexpectedGuests Cards.unexpectedGuests (Static 6)

instance HasModifiersFor UnexpectedGuests where
  getModifiersFor (UnexpectedGuests a) =
    when (onSide A a) $ modifySelect a Anyone [CannotParleyWith $ EnemyWithTrait LanternClub]

instance RunMessage UnexpectedGuests where
  runMessage msg (UnexpectedGuests attrs) = runQueueT $ case msg of
    -- TODO handle spellbound forced ability and agenda advancement
    _ -> UnexpectedGuests <$> liftRunMessage msg attrs
