module Arkham.Enemy.Cards.YourWorstNightmare (yourWorstNightmare) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype YourWorstNightmare = YourWorstNightmare EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

yourWorstNightmare :: EnemyCard YourWorstNightmare
yourWorstNightmare =
  enemy YourWorstNightmare Cards.yourWorstNightmare (2, Static 3, 2) (0, 2) & setPreyIsOnlyBearer

instance HasModifiersFor YourWorstNightmare where
  getModifiersFor (YourWorstNightmare a) = do
    for_ (enemyBearer a) \iid -> do
      modified_ a iid [CannotTakeAction $ EnemyAction #fight $ EnemyWithId a.id]
      modifySelf
        a
        [ CannotBeDamagedByPlayerSources (SourceOwnedBy $ InvestigatorWithId iid)
        , CanOnlyBeDefeatedBy (NotSource $ SourceOwnedBy $ InvestigatorWithId iid)
        ]

instance RunMessage YourWorstNightmare where
  runMessage msg (YourWorstNightmare attrs) =
    YourWorstNightmare <$> runMessage msg attrs
