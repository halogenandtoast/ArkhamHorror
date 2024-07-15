module Arkham.Event.Cards.StringOfCurses (stringOfCurses, StringOfCurses (..)) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message.Lifted.Choose
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
          labeled
            "Automatically evade that enemy and place 1 doom on it. It cannot take damage for the remainder of the round. Discover 1 clue at your location."
            do
              push $ EnemyEvaded iid eid
              placeDoom attrs eid 1
              roundModifier attrs eid CannotBeDamaged
              discoverAtYourLocation NotInvestigate iid attrs 1
        when option2 do
          labeled
            "If that enemy has 1 or more doom on it, defeat it. Gain 1 resource for each doom that was on it."
            do
              defeatEnemy eid iid attrs
              gainResourcesIfCan iid attrs doom

      pure e
    _ -> StringOfCurses <$> liftRunMessage msg attrs
