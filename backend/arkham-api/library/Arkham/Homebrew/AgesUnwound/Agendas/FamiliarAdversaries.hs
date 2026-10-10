module Arkham.Homebrew.AgesUnwound.Agendas.FamiliarAdversaries (familiarAdversaries) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards

newtype FamiliarAdversaries = FamiliarAdversaries AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Agenda 2. "Locations with a resource on them are 'warded'." is the same
reminder acts 1 and 2 print and needs nothing of the agenda.

TODO(ages-unwound): "[[Fate]] treacheries cannot be cancelled or discarded from
play, except by abilities printed on them" is not implemented. The engine has no
treachery-side "cannot be discarded from play" or "cannot be cancelled" modifier
-- 'Arkham.Modifier.CannotBeRemovedBy' is read only by the enemy runner, and the
cancel modifiers that exist are about chaos tokens, attacks and effects, not about
a card in play refusing to leave it. Implementing it needs a shared primitive, so
it is reported rather than invented here. The clause protects /Unwritten
Existence/ and /Aged a Thousand Years/, both of which print their own way off the
table.
-}
familiarAdversaries :: AgendaCard FamiliarAdversaries
familiarAdversaries = agenda (2, A) FamiliarAdversaries Cards.familiarAdversaries (Static 7)

-- | Agenda 2b /A Broken Eternity/: "(->R1)."
instance RunMessage FamiliarAdversaries where
  runMessage msg a@(FamiliarAdversaries attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R1
      pure a
    _ -> FamiliarAdversaries <$> liftRunMessage msg attrs
