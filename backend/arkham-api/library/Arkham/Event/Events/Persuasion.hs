module Arkham.Event.Events.Persuasion (persuasion) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Projection
import Arkham.Taboo
import Arkham.Trait

newtype Persuasion = Persuasion EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

persuasion :: EventCard Persuasion
persuasion = event Persuasion Cards.persuasion

instance RunMessage Persuasion where
  runMessage msg e@(Persuasion attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      field InvestigatorLocation iid >>= \case
        Just location -> do
          let tabooMatcher = if tabooed TabooList21 attrs then id else (<> EnemyWithTrait Humanoid)

          enemies <- select $ tabooMatcher $ enemyAt location <> NonWeaknessEnemy <> canParleyEnemy iid
          sid <- getRandom
          chooseTargetM iid enemies \enemy ->
            parley sid iid attrs enemy #intellect
              $ SumCalculation [Fixed 3, EnemyFieldCalculation enemy EnemySanityDamage]
        _ -> error "investigator not at location"
      pure e
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      getSkillTestTargetedEnemy >>= \case
        Just eid -> do
          isElite <- eid <=~> EliteEnemy
          if isElite
            then push $ EnemyEvaded iid eid
            else push $ ShuffleBackIntoEncounterDeck (EnemyTarget eid)
        _ -> pure ()
      pure e
    _ -> Persuasion <$> liftRunMessage msg attrs
