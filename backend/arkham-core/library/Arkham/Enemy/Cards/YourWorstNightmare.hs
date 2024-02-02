module Arkham.Enemy.Cards.YourWorstNightmare (
  yourWorstNightmare,
  YourWorstNightmare (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype YourWorstNightmare = YourWorstNightmare EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

yourWorstNightmare :: EnemyCard YourWorstNightmare
yourWorstNightmare = enemyWith YourWorstNightmare Cards.yourWorstNightmare (2, Static 3, 2) (0, 2)
  $ \a -> a & preyL .~ BearerOf (toId a)

instance HasModifiersFor YourWorstNightmare where
  getModifiersFor (InvestigatorTarget iid) (YourWorstNightmare attrs) | enemyBearer attrs == Just iid = do
    pure $ toModifiers attrs [CannotTakeAction $ EnemyAction #fight $ EnemyWithId $ toId attrs]
  getModifiersFor target (YourWorstNightmare attrs) | attrs `is` target = do
    pure
      $ toModifiers
        attrs
        [ CannotBeDamagedByPlayerSources
            (SourceOwnedBy $ InvestigatorWithId $ fromJustNote "must have bearer" $ enemyBearer attrs)
        , CanOnlyBeDefeatedBy
            ( NotSource
                $ SourceOwnedBy
                $ InvestigatorWithId
                $ fromJustNote "must have bearer"
                $ enemyBearer attrs
            )
        ]
  getModifiersFor _ _ = pure []

instance RunMessage YourWorstNightmare where
  runMessage msg (YourWorstNightmare attrs) =
    YourWorstNightmare <$> runMessage msg attrs
