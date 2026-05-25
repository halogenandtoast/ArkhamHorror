module Arkham.Act.Cards.FaceToCarapace (faceToCarapace) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.ForMovement
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Dark))

newtype FaceToCarapace = FaceToCarapace ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

faceToCarapace :: ActCard FaceToCarapace
faceToCarapace = act (3, A) FaceToCarapace Cards.faceToCarapace Nothing

limulusHybrid :: EnemyMatcher
limulusHybrid = oneOf [enemyIs Enemies.limulusHybridInTheLight, enemyIs Enemies.limulusHybridInTheDark]

instance HasAbilities FaceToCarapace where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1
        $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) Anywhere)
    , mkAbility a 2
        $ Objective
        $ forced
        $ IfEnemyDefeated #after Anyone ByAny limulusHybrid
    ]

instance RunMessage FaceToCarapace where
  runMessage msg a@(FaceToCarapace attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ EnemyAt (ConnectedFrom NotForMovement (locationWithInvestigator iid) <> LocationWithTrait Dark)
      chooseTargetM iid enemies \eid -> do
        enemyMoveTo attrs eid =<< selectJust (locationWithInvestigator iid)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> FaceToCarapace <$> liftRunMessage msg attrs
