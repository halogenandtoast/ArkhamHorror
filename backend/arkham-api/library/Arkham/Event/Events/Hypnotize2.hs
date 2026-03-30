module Arkham.Event.Events.Hypnotize2 (hypnotize2) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher

newtype Hypnotize2 = Hypnotize2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hypnotize2 :: EventCard Hypnotize2
hypnotize2 = event Hypnotize2 Cards.hypnotize2

instance RunMessage Hypnotize2 where
  runMessage msg e@(Hypnotize2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      enemies <-
        select
          $ EnemyAt (locationWithInvestigator iid)
          <> NonEliteEnemy
          <> canParleyEnemy iid
          <> EnemyWithHealth
      sid <- getRandom
      modifySkill sid attrs iid #intellect 2
      chooseTargetM iid enemies \enemy -> do
        parley sid iid attrs enemy #intellect $ EnemyMaybeFieldCalculation enemy EnemyRemainingHealth
      pure e
    PassedThisSkillTest _ (isSource attrs -> True) -> do
      withSkillTestTargetedEnemy shuffleBackIntoEncounterDeck
      pure e
    _ -> Hypnotize2 <$> liftRunMessage msg attrs
