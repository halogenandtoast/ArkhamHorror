module Arkham.Treachery.Cards.DanceOfTheYellowKing (danceOfTheYellowKing, DanceOfTheYellowKing (..)) where

import Arkham.Attack
import Arkham.Classes
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DanceOfTheYellowKing = DanceOfTheYellowKing TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

danceOfTheYellowKing :: TreacheryCard DanceOfTheYellowKing
danceOfTheYellowKing = treachery DanceOfTheYellowKing Cards.danceOfTheYellowKing

instance RunMessage DanceOfTheYellowKing where
  runMessage msg t@(DanceOfTheYellowKing attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      anyLunatics <- selectAny $ EnemyWithTrait Lunatic
      sid <- getRandom
      push
        $ if anyLunatics
          then revelationSkillTest sid iid attrs #willpower (Fixed 3)
          else gainSurge attrs
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      lunatics <- select $ NearestEnemy $ EnemyWithTrait Lunatic
      withLocationOf iid \lid -> do
        player <- getPlayer iid
        push
          $ chooseOrRunOne
            player
            [ targetLabel
              lunatic
              [ Ready (toTarget lunatic)
              , MoveUntil lid (toTarget lunatic)
              , IfEnemyExists
                  (enemyAtLocationWith iid <> EnemyWithId lunatic)
                  [ EnemyEngageInvestigator lunatic iid
                  , EnemyWillAttack $ enemyAttack lunatic attrs iid
                  ]
              ]
            | lunatic <- lunatics
            ]
      pure t
    _ -> DanceOfTheYellowKing <$> runMessage msg attrs
