module Arkham.Act.Cards.QuestioningTheGangsV2 (questioningTheGangsV2) where

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

newtype QuestioningTheGangsV2 = QuestioningTheGangsV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

questioningTheGangsV2 :: ActCard QuestioningTheGangsV2
questioningTheGangsV2 = act (1, A) QuestioningTheGangsV2 Cards.questioningTheGangsV2 Nothing

instance HasAbilities QuestioningTheGangsV2 where
  getAbilities (QuestioningTheGangsV2 a) | onSide A a =
    [ restricted a 1 (youExist $ InvestigatorEngagedWith (EnemyWithTrait Criminal))
        $ FastAbility Free
    , mkAbility a 2 $ Objective $ forced $ RoundEnds #when
    ]
  getAbilities _ = []

instance RunMessage QuestioningTheGangsV2 where
  runMessage msg a@(QuestioningTheGangsV2 attrs) = runQueueT $ case msg of
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
      laBellaLunaId <- placeLocation =<< genCard Locations.laBellaLunaTheDrownedCity
      downtown <- selectJust (LocationWithTitle "Downtown")
      connectBothWays laBellaLunaId downtown
      naomi <- getSetAsideCard Enemies.naomiOBannion
      createEnemyAt_ naomi laBellaLunaId
      advanceToAct attrs Cards.theOBannionGang A
      pure a
    _ -> QuestioningTheGangsV2 <$> liftRunMessage msg attrs
