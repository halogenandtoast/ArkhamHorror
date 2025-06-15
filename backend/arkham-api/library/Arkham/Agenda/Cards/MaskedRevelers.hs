module Arkham.Agenda.Cards.MaskedRevelers (
  maskedRevelers,
  MaskedRevelers(..),
) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (modifySelect)
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Trait

newtype MaskedRevelers = MaskedRevelers AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maskedRevelers :: AgendaCard MaskedRevelers
maskedRevelers = agenda (1, A) MaskedRevelers Cards.maskedRevelers (Static 4)

instance HasModifiersFor MaskedRevelers where
  getModifiersFor (MaskedRevelers a) =
    when (onSide A a) $ modifySelect a (EnemyWithTrait LanternClub) [CannotBeDamaged]

instance RunMessage MaskedRevelers where
  runMessage msg a@(MaskedRevelers attrs) = runQueueT $ case msg of
    -- TODO handle spellbound forced ability and agenda advancement
    _ -> MaskedRevelers <$> liftRunMessage msg attrs
