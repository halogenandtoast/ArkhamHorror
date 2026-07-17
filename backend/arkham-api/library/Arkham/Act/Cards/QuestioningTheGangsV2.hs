module Arkham.Act.Cards.QuestioningTheGangsV2 (questioningTheGangsV2) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyForcedRemainingHealth))
import Arkham.Helpers.Location (connectBothWays)
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection (field)
import Arkham.Trait (Trait (Criminal))

newtype QuestioningTheGangsV2 = QuestioningTheGangsV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

questioningTheGangsV2 :: ActCard QuestioningTheGangsV2
questioningTheGangsV2 = act (1, A) QuestioningTheGangsV2 Cards.questioningTheGangsV2 Nothing

instance HasAbilities QuestioningTheGangsV2 where
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
        (FastAbility' Free #parley)
    , restricted a 2 (CluesOnThis $ AtLeast $ StaticWithPerPlayer 2 1)
        $ Objective
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage QuestioningTheGangsV2 where
  runMessage msg a@(QuestioningTheGangsV2 attrs) = runQueueT $ case msg of
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
      laBellaLuna <- placeLocation Locations.laBellaLunaTheDrownedCity
      withMatch (LocationWithTitle "Downtown") (connectBothWays laBellaLuna)
      createEnemyAt_ Enemies.naomiOBannion laBellaLuna
      setActDeck [Cards.questioningTheGangsV2, Cards.theOBannionGang, Cards.faceTheMusic]
      advanceActDeck attrs
      pure a
    _ -> QuestioningTheGangsV2 <$> liftRunMessage msg attrs
