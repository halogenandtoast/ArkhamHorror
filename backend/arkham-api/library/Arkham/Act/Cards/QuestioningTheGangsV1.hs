module Arkham.Act.Cards.QuestioningTheGangsV1 (questioningTheGangsV1) where

import Arkham.Ability
import Arkham.Act.CardDefs.TheDrownedCity qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.CardDefs.TheDrownedCity qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyForcedRemainingHealth))
import Arkham.Helpers.Location (connectBothWays)
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Location.CardDefs.TheDrownedCity qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection (field)
import Arkham.Trait (Trait (Criminal))

newtype QuestioningTheGangsV1 = QuestioningTheGangsV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

questioningTheGangsV1 :: ActCard QuestioningTheGangsV1
questioningTheGangsV1 = act (1, A) QuestioningTheGangsV1 Cards.questioningTheGangsV1 Nothing

instance HasAbilities QuestioningTheGangsV1 where
  getAbilities = actAbilities \a ->
    [ restricted
        a
        1
        ( youExist
            $ InvestigatorEngagedWith
            $ EnemyWithTrait Criminal
            <> EnemyWithRemainingHealthLessThan
              (SumCalculation [Fixed 1, InvestigatorsFieldCalculation You InvestigatorClues])
        )
        freeTrigger_
    , restricted a 2 (CluesOnThis $ AtLeast $ StaticWithPerPlayer 2 1)
        $ Objective
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage QuestioningTheGangsV1 where
  runMessage msg a@(QuestioningTheGangsV1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      clues <- field InvestigatorClues iid
      criminals <-
        select
          $ enemyEngagedWith iid
          <> EnemyWithTrait Criminal
          <> EnemyWithRemainingHealthLessThan (Fixed $ 1 + clues)
      chooseHandleTargetM iid (attrs.ability 1) criminals
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (EnemyTarget enemy) -> do
      spendClues iid =<< field EnemyForcedRemainingHealth enemy
      toDiscardBy iid (attrs.ability 1) enemy
      placeClues (attrs.ability 1) attrs.id 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      hibbsRoadhouse <- placeLocation Locations.hibbsRoadhouse
      withMatch (LocationWithTitle "Easttown") (connectBothWays hibbsRoadhouse)
      createEnemyAt_ Enemies.sadieSheldon hibbsRoadhouse
      setActDeck [Cards.questioningTheGangsV1, Cards.theSheldonGang, Cards.faceTheMusic]
      advanceActDeck attrs
      pure a
    _ -> QuestioningTheGangsV1 <$> liftRunMessage msg attrs
