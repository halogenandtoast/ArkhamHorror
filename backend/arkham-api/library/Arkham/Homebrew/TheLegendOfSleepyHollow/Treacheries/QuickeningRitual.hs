module Arkham.Homebrew.TheLegendOfSleepyHollow.Treacheries.QuickeningRitual (
  quickeningRitual,
) where

import Arkham.Ability
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Witch))
import Arkham.Treachery.Import.Lifted

newtype QuickeningRitual = QuickeningRitual TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickeningRitual :: TreacheryCard QuickeningRitual
quickeningRitual = treachery QuickeningRitual Cards.quickeningRitual

instance HasAbilities QuickeningRitual where
  getAbilities (QuickeningRitual a) =
    [ scenarioI18n
        $ withI18nTooltip "quickeningRitual.surge"
        $ restricted a 1 (InThreatAreaOf You)
        $ forced
        $ GainsSurge #when You AnySource
    , scenarioI18n
        $ withI18nTooltip "quickeningRitual.break"
        $ skillTestAbility
        $ restricted a 2 (InThreatAreaOf You) actionAbility
    ]

instance RunMessage QuickeningRitual where
  runMessage msg t@(QuickeningRitual attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      alreadyThere <- selectAny $ treacheryInThreatAreaOf iid <> treacheryIs Cards.quickeningRitual
      if alreadyThere then gainSurge attrs else placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ withI18n do
        countVar 1 $ labeled "takeDamage" $ assignDamage iid (attrs.ability 1) 1
        countVar 1 $ labeled "takeHorror" $ assignHorror iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      whenAny (at_ (locationWithInvestigator iid) <> ExhaustedEnemy <> EnemyWithTrait Witch)
        $ skillTestAutomaticallySucceeds (attrs.ability 2) sid
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> QuickeningRitual <$> liftRunMessage msg attrs
