module Arkham.Act.Cards.TheNightsUsurper (theNightsUsurper) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Act.Sequence
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Log (remembered)
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.ScenarioLogKey

newtype TheNightsUsurper = TheNightsUsurper ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNightsUsurper :: ActCard TheNightsUsurper
theNightsUsurper = act (3, A) TheNightsUsurper Cards.theNightsUsurper Nothing

instance HasAbilities TheNightsUsurper where
  getAbilities (TheNightsUsurper a) =
    [ playerLimit PerPhase
        $ restricted
          a
          1
          (youExist $ InvestigatorAt $ LocationWithEnemy $ enemyIs Enemies.xzharah <> ExhaustedEnemy)
        $ Objective
        $ FastAbility Free
    ]

instance RunMessage TheNightsUsurper where
  runMessage msg a@(TheNightsUsurper attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      mXzharah <- selectOne $ enemyIs Enemies.xzharah
      xzharahDefeated <- case mXzharah of
        Nothing -> pure True
        Just xzharah -> do
          health <- fieldMap EnemyRemainingHealth (fromMaybe 0) xzharah
          pure $ health <= 0
      investigators <- getInvestigators
      totalClues <- getSpendableClueCount investigators
      requiredClues <- perPlayer 3
      strength <- getStrengthOfTheAbyss
      tasksDone <-
        andM
          [ remembered FreedTheNightgaunts
          , remembered WarnedTheDenizensOfSarkomand
          , remembered PledForHelp
          ]
      let
        requirementsMet =
          count id [xzharahDefeated, totalClues >= requiredClues, strength <= 4, tasksDone]
      lead <- getLead
      if requirementsMet >= 3
        then campaignI18n $ chooseOneM lead do
          labeled' "defeatingXzharah.proceed" $ push R2
          labeled' "defeatingXzharah.flipBack" $ push $ RevertAct attrs.id
        else push $ RevertAct attrs.id
      pure a
    RevertAct aid | aid == attrs.id && onSide B attrs -> do
      pure $ TheNightsUsurper $ attrs & sequenceL .~ Sequence 3 A
    _ -> TheNightsUsurper <$> liftRunMessage msg attrs
