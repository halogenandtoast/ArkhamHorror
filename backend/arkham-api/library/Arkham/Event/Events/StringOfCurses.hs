module Arkham.Event.Events.StringOfCurses (stringOfCurses) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.I18n
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Modifier
import Arkham.Projection

newtype StringOfCurses = StringOfCurses EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stringOfCurses :: EventCard StringOfCurses
stringOfCurses = event StringOfCurses Cards.stringOfCurses

instance RunMessage StringOfCurses where
  runMessage msg e@(StringOfCurses attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectWithNonNull
        ( NonEliteEnemy
            <> EnemyAt YourLocation
            <> canParleyEnemy iid
            <> oneOf
              [EnemyCanBeEvadedBy (toSource attrs), EnemyWithAnyDoom <> EnemyCanBeDefeatedBy (toSource attrs)]
        )
        $ chooseOneToHandle iid attrs
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      option1 <- eid <=~> EnemyCanBeEvadedBy (toSource attrs)
      option2 <- eid <=~> (EnemyWithAnyDoom <> EnemyCanBeDefeatedBy (toSource attrs))
      doom <- field EnemyDoom eid
      chooseOneM iid do
        when option1 do
          (cardI18n $ labeled "stringOfCurses.evadeAndPlaceDoom") do
            automaticallyEvadeEnemy iid eid
            placeDoom attrs eid 1
            roundModifier attrs eid CannotBeDamaged
            discoverAtYourLocation NotInvestigate iid attrs 1
        when option2 do
          (cardI18n $ labeled "stringOfCurses.defeatForResources") do
            defeatEnemy eid iid attrs
            gainResourcesIfCan iid attrs doom

      pure e
    _ -> StringOfCurses <$> liftRunMessage msg attrs
