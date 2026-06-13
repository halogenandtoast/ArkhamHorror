module Arkham.Act.Cards.QuestioningTheGangsV1 (questioningTheGangsV1) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card (genCard)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyForcedRemainingHealth))
import Arkham.Helpers.Location (connectBothWays)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose (chooseTargetM)
import Arkham.Projection (field)
import Arkham.Trait (Trait (Criminal))

newtype QuestioningTheGangsV1 = QuestioningTheGangsV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

questioningTheGangsV1 :: ActCard QuestioningTheGangsV1
questioningTheGangsV1 = act (1, A) QuestioningTheGangsV1 Cards.questioningTheGangsV1 Nothing

instance HasAbilities QuestioningTheGangsV1 where
  getAbilities (QuestioningTheGangsV1 a) | onSide A a =
    [ restricted a 1 (youExist $ InvestigatorEngagedWith (EnemyWithTrait Criminal))
        $ FastAbility Free
    , mkAbility a 2 $ Objective $ forced $ RoundEnds #when
    ]
  getAbilities _ = []

instance RunMessage QuestioningTheGangsV1 where
  runMessage msg a@(QuestioningTheGangsV1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      criminals <- select $ enemyEngagedWith iid <> EnemyWithTrait Criminal
      chooseTargetM iid criminals \enemy -> do
        h <- field EnemyForcedRemainingHealth enemy
        spendClues iid h
        toDiscardBy iid (attrs.ability 1) enemy
        placeClues (attrs.ability 1) attrs.id 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      threshold <- (2 +) <$> getPlayerCount
      when (actClues attrs >= threshold) $ advancedWithOther attrs
      pure a
    AdvanceAct (isSide A attrs -> True) _ _ -> do
      hibbId <- placeLocation =<< genCard Locations.hibbsRoadhouse
      easttown <- selectJust (LocationWithTitle "Easttown")
      connectBothWays hibbId easttown
      sadie <- getSetAsideCard Enemies.sadieSheldon
      createEnemyAt_ sadie hibbId
      advanceToAct attrs Cards.theSheldonGang A
      pure a
    _ -> QuestioningTheGangsV1 <$> liftRunMessage msg attrs
