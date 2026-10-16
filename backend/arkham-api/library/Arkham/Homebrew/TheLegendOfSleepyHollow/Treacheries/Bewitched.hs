module Arkham.Homebrew.TheLegendOfSleepyHollow.Treacheries.Bewitched (bewitched) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.Matcher
import Arkham.Trait (Trait (Witch))
import Arkham.Treachery.Import.Lifted

newtype Bewitched = Bewitched TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bewitched :: TreacheryCard Bewitched
bewitched = treachery Bewitched Cards.bewitched

-- | "Ignore the first skill icon committed to all skill tests you perform."
instance HasModifiersFor Bewitched where
  getModifiersFor (Bewitched a) = inThreatAreaGets a [FewerMatchingIcons 1]

instance HasAbilities Bewitched where
  getAbilities (Bewitched a) =
    [ scenarioI18n
        $ withI18nTooltip "bewitched.break"
        $ skillTestAbility
        $ restricted a 1 (InThreatAreaOf You) actionAbility
    ]

instance RunMessage Bewitched where
  runMessage msg t@(Bewitched attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      alreadyThere <- selectAny $ treacheryInThreatAreaOf iid <> treacheryIs Cards.bewitched
      if alreadyThere then gainSurge attrs else placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      whenAny (at_ (locationWithInvestigator iid) <> ExhaustedEnemy <> EnemyWithTrait Witch)
        $ skillTestAutomaticallySucceeds (attrs.ability 1) sid
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> Bewitched <$> liftRunMessage msg attrs
