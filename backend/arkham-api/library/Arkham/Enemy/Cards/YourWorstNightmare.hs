module Arkham.Enemy.Cards.YourWorstNightmare (yourWorstNightmare, YourWorstNightmare (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype YourWorstNightmare = YourWorstNightmare EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

yourWorstNightmare :: EnemyCard YourWorstNightmare
yourWorstNightmare = enemyWith YourWorstNightmare Cards.yourWorstNightmare (2, Static 3, 2) (0, 2)
  $ \a -> a & preyL .~ BearerOf (toId a)

instance HasModifiersFor YourWorstNightmare where
  getModifiersFor (YourWorstNightmare a) = do
    bearer <- case enemyBearer a of
      Nothing -> pure mempty
      Just iid -> modified_ a iid [CannotTakeAction $ EnemyAction #fight $ EnemyWithId a.id]
    self <-
      modifySelf
        a
        [ CannotBeDamagedByPlayerSources
            (SourceOwnedBy $ InvestigatorWithId $ fromJustNote "must have bearer" $ enemyBearer a)
        , CanOnlyBeDefeatedBy
            ( NotSource
                $ SourceOwnedBy
                $ InvestigatorWithId
                $ fromJustNote "must have bearer"
                $ enemyBearer a
            )
        ]
    pure $ bearer <> self

instance RunMessage YourWorstNightmare where
  runMessage msg (YourWorstNightmare attrs) =
    YourWorstNightmare <$> runMessage msg attrs
