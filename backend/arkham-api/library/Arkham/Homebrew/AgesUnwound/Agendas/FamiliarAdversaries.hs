module Arkham.Homebrew.AgesUnwound.Agendas.FamiliarAdversaries (familiarAdversaries) where

import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.Traits (pattern Fate)
import Arkham.Matcher

newtype FamiliarAdversaries = FamiliarAdversaries AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Agenda 2. "Locations with a resource on them are 'warded'." is the same
reminder acts 1 and 2 print and needs nothing of the agenda.

"[[Fate]] treacheries cannot be cancelled or discarded from play, except by
abilities printed on them." The discard half is 'CannotLeavePlay', which the
treachery discard in @Game/Runner@ now honours the way the asset runner always
has -- including the "except by abilities printed on them" exemption, since a
discard a treachery sources itself is let through. It protects /Unwritten
Existence/ and /Aged a Thousand Years/, both of which print their own way off
the table.

TODO(ages-unwound): the /cancelled/ half is still open. 'EffectsCannotBeCanceled'
is the right modifier and the cancel matchers read it off the card
(@Game.hs@'s @CanCancelAllEffects@), but a treachery is cancelled as it is
drawn, before it is an entity to hang a modifier on -- so it would have to be
applied to every Fate card wherever it lives, and enumerating those on a
'HasModifiersFor' pass that runs constantly is the wrong shape. It wants a
card-matcher-scoped modifier the cancel check consults directly.
-}
familiarAdversaries :: AgendaCard FamiliarAdversaries
familiarAdversaries = agenda (2, A) FamiliarAdversaries Cards.familiarAdversaries (Static 7)

instance HasModifiersFor FamiliarAdversaries where
  getModifiersFor (FamiliarAdversaries a) =
    modifySelect a (TreacheryWithTrait Fate) [CannotLeavePlay]

-- | Agenda 2b /A Broken Eternity/: "(->R1)."
instance RunMessage FamiliarAdversaries where
  runMessage msg a@(FamiliarAdversaries attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R1
      pure a
    _ -> FamiliarAdversaries <$> liftRunMessage msg attrs
