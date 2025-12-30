module Arkham.Event.Events.Interrogate (interrogate) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher
import Arkham.Taboo
import Arkham.Trait (Trait (Humanoid))

newtype Interrogate = Interrogate EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

interrogate :: EventCard Interrogate
interrogate = event Interrogate Cards.interrogate

instance RunMessage Interrogate where
  runMessage msg e@(Interrogate attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      withLocationOf iid \location -> do
        let tabooMatcher = if tabooed TabooList21 attrs then id else (<> EnemyWithTrait Humanoid)
        enemies <- select $ tabooMatcher $ enemyAt location <> canParleyEnemy iid
        sid <- getRandom
        chooseTargetM iid enemies \enemy ->
          parley sid iid attrs enemy #combat
            $ SumCalculation [Fixed 3, EnemyFieldCalculation enemy EnemyHealthDamage]
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      matcher <-
        getLocationOf iid <&> \case
          Nothing -> LocationWithAnyClues
          Just lid -> LocationWithAnyClues <> NotLocation (LocationWithId lid)
      discoverAtYourLocation NotInvestigate iid attrs 1
      discoverAtMatchingLocation NotInvestigate iid attrs matcher 1
      pure e
    _ -> Interrogate <$> liftRunMessage msg attrs
