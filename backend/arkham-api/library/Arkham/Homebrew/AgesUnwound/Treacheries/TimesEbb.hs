module Arkham.Homebrew.AgesUnwound.Treacheries.TimesEbb (timesEbb) where

import Arkham.Ability
import Arkham.Effect.Builder
import Arkham.Effect.Window
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype TimesEbb = TimesEbb TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timesEbb :: TreacheryCard TimesEbb
timesEbb = treachery TimesEbb Cards.timesEbb

{- | "Revelation - Put Time's Ebb into play in your threat area. /
Forced - At the start of the mythos phase: You may choose an investigator. If
you do not, lose an action. If you do, discard Time's Ebb, and the next time the
chosen investigator draws an encounter card, they then draw the top two cards of
the encounter deck."

The card discards itself the moment a choice is made, which is what it prints,
so the delayed draw cannot be an ability of its own -- a discarded treachery is
no longer an entity to hang one on. It is a generic effect instead, scoped to
'EffectNextEncounterDrawWindow' and carrying its two draws as 'onDisable'. The
window ends on the draw rather than in front of it, so the extra cards come
after the card that triggered them; an ability could only have fired in the
@#when@ window, which is the only one an encounter draw emits.
-}
instance HasAbilities TimesEbb where
  getAbilities (TimesEbb a) =
    [restricted a 1 (InThreatAreaOf You) $ forced $ PhaseBegins #when #mythos]

instance RunMessage TimesEbb where
  runMessage msg t@(TimesEbb attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select Anyone
      chooseOneM iid $ campaignI18n do
        for_ investigators \i -> targeting i $ handleTarget iid (attrs.ability 1) i
        labeled "timesEbb.doNotChoose" $ loseStandardActions iid (attrs.ability 1) 1
      pure t
    HandleTargetChoice _ (isAbilitySource attrs 1 -> True) (InvestigatorTarget who) -> do
      withSource (attrs.ability 1) $ effect who do
        removeOn $ EffectNextEncounterDrawWindow who
        onDisable $ drawEncounterCards who (attrs.ability 1) 2
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> TimesEbb <$> liftRunMessage msg attrs
