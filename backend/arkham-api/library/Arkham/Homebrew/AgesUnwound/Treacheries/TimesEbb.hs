module Arkham.Homebrew.AgesUnwound.Treacheries.TimesEbb (timesEbb) where

import Arkham.Ability
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

-- | The investigator chosen by ability 1, whose next encounter draw ability 2 rides.
chosen :: TreacheryAttrs -> Maybe InvestigatorId
chosen a = toResultDefault Nothing a.meta

{- | "Revelation - Put Time's Ebb into play in your threat area. /
Forced - At the start of the mythos phase: You may choose an investigator. If
you do not, lose an action. If you do, discard Time's Ebb, and the next time the
chosen investigator draws an encounter card, they then draw the top two cards of
the encounter deck."

TODO(ages-unwound): the printed text discards Time's Ebb the moment an
investigator is chosen, but a discarded treachery is no longer an entity and
there is no primitive for a one-shot delayed trigger -- no @EffectWindow@ for
"the next encounter draw", and homebrew campaigns have no effect-module seam
(@Arkham.Effect.hs@ is the registry and is off limits). So the card stays in the
threat area as its own host until the delayed draw fires, then discards itself.
It prints no ongoing penalty, and ability 1 cannot re-open once a choice is
recorded, so the only observable difference is that it is still a treachery in
your threat area for one window longer.

The extra two cards are drawn in the @#when@ window of the triggering draw --
the only encounter-draw window the engine emits (@Game/Runner.hs:3784@ pushes
@Window.DrawCard@ with @mkWhen@ only) -- so they resolve just before the card
that triggered them rather than just after.
-}
instance HasAbilities TimesEbb where
  getAbilities (TimesEbb a) = case chosen a of
    Nothing -> [restricted a 1 (InThreatAreaOf You) $ forced $ PhaseBegins #when #mythos]
    Just who ->
      [mkAbility a 2 $ SilentForcedAbility $ DrawCard #when (be who) (basic AnyCard) EncounterDeck]

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
    HandleTargetChoice _ (isAbilitySource attrs 1 -> True) (InvestigatorTarget who) ->
      pure $ setMeta (Just who) t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      for_ (chosen attrs) \who -> drawEncounterCards who (attrs.ability 2) 2
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> TimesEbb <$> liftRunMessage msg attrs
