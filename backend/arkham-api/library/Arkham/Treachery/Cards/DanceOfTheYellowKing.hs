module Arkham.Treachery.Cards.DanceOfTheYellowKing (danceOfTheYellowKing) where

import Arkham.Attack
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DanceOfTheYellowKing = DanceOfTheYellowKing TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

danceOfTheYellowKing :: TreacheryCard DanceOfTheYellowKing
danceOfTheYellowKing = treachery DanceOfTheYellowKing Cards.danceOfTheYellowKing

instance RunMessage DanceOfTheYellowKing where
  runMessage msg t@(DanceOfTheYellowKing attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      anyLunatics <- selectAny $ EnemyWithTrait Lunatic
      sid <- getRandom
      if anyLunatics
        then revelationSkillTest sid iid attrs #willpower (Fixed 3)
        else gainSurge attrs
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      lunatics <- select $ NearestEnemyTo iid $ EnemyWithTrait Lunatic
      withLocationOf iid \lid -> do
        chooseOrRunOneM iid do
          targets lunatics \lunatic -> do
            readyThis lunatic
            push $ MoveUntil lid (toTarget lunatic)
            push
              $ IfEnemyExists
                (enemyAtLocationWith iid <> EnemyWithId lunatic)
                [ EnemyEngageInvestigator lunatic iid
                , EnemyWillAttack $ enemyAttack lunatic attrs iid
                ]
      pure t
    _ -> DanceOfTheYellowKing <$> liftRunMessage msg attrs
