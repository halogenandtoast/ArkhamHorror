module Arkham.Treachery.Cards.DanceOfTheYellowKing (
  danceOfTheYellowKing,
  DanceOfTheYellowKing (..),
) where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DanceOfTheYellowKing = DanceOfTheYellowKing TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

danceOfTheYellowKing :: TreacheryCard DanceOfTheYellowKing
danceOfTheYellowKing =
  treachery DanceOfTheYellowKing Cards.danceOfTheYellowKing

instance RunMessage DanceOfTheYellowKing where
  runMessage msg t@(DanceOfTheYellowKing attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      anyLunatics <- selectAny $ EnemyWithTrait Lunatic
      push
        $ if anyLunatics
          then RevelationSkillTest iid source SkillWillpower 3
          else gainSurge attrs
      pure t
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ _ | isSource attrs source -> do
      lunatics <- selectList $ NearestEnemy $ EnemyWithTrait Lunatic
      mlid <- field InvestigatorLocation iid
      for_ mlid $ \lid -> do
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
