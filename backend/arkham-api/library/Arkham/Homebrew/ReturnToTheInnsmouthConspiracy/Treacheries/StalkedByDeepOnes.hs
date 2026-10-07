module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Treacheries.StalkedByDeepOnes (stalkedByDeepOnes) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (DeepOne))
import Arkham.Treachery.Import.Lifted

newtype StalkedByDeepOnes = StalkedByDeepOnes TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stalkedByDeepOnes :: TreacheryCard StalkedByDeepOnes
stalkedByDeepOnes = treachery StalkedByDeepOnes Cards.stalkedByDeepOnes

-- | "You gain the Deep One trait." This is what makes you a Deep One investigator.
instance HasModifiersFor StalkedByDeepOnes where
  getModifiersFor (StalkedByDeepOnes a) = for_ a.inThreatAreaOf \iid ->
    modified_ a iid [AddTrait DeepOne]

instance HasAbilities StalkedByDeepOnes where
  getAbilities (StalkedByDeepOnes a) =
    [ skillTestAbility
        $ restricted a 1 (InThreatAreaOf You <> youExist (InvestigatorEngagedWith $ EnemyWithTrait DeepOne))
        $ forced
        $ PhaseBegins #when #investigation
    ]

instance RunMessage StalkedByDeepOnes where
  runMessage msg t@(StalkedByDeepOnes attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      enemies <- select $ EnemyWithTrait DeepOne <> enemyEngagedWith iid
      chooseOrRunOneM iid $ targets enemies \enemy -> do
        disengageEnemy iid enemy
        enemyCheckEngagement enemy
      pure t
    _ -> StalkedByDeepOnes <$> liftRunMessage msg attrs
