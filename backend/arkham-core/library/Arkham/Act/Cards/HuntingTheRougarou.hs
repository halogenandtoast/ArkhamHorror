module Arkham.Act.Cards.HuntingTheRougarou where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Asset.Types (Field (..))
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher
import Arkham.Message hiding (EnemyDamage, EnemyDefeated)
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Types (Field (..))
import Arkham.ScenarioLogKey
import Arkham.Scenarios.CurseOfTheRougarou.Helpers
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype HuntingTheRougarou = HuntingTheRougarou ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingTheRougarou :: ActCard HuntingTheRougarou
huntingTheRougarou =
  act (2, A) HuntingTheRougarou Cards.huntingTheRougarou Nothing

instance HasAbilities HuntingTheRougarou where
  getAbilities (HuntingTheRougarou a) =
    [ mkAbility a 1 $
        ForcedAbility $
          EnemyLeaves Timing.After Anywhere $
            enemyIs Cards.theRougarou
    , mkAbility a 2 $
        Objective $
          ForcedAbility $
            EnemyDefeated Timing.When Anyone ByAny $
              enemyIs Cards.theRougarou
    , limitedAbility (PlayerLimit PerPhase 1) $
        restrictedAbility
          a
          3
          ( InvestigatorExists $
              You
                <> InvestigatorEngagedWith
                  (enemyIs Cards.theRougarou)
          )
          (Objective $ FastAbility Free)
    ]

instance RunMessage HuntingTheRougarou where
  runMessage msg a@(HuntingTheRougarou attrs) = case msg of
    UseCardAbility _ source 1 [Window _ (Window.EnemyLeaves _ lid)] _
      | isSource attrs source -> a <$ push (PlaceClues (toAbilitySource attrs 1) (toTarget lid) 1)
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      a <$ push (ScenarioResolution $ Resolution 2)
    UseCardAbility _ source 3 _ _ | isSource attrs source -> do
      a <$ push (AdvanceAct (toId attrs) source AdvancedWithOther)
    AdvanceAct aid _ _ | aid == toId attrs && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      rougarou <- fromJustNote "must be" <$> getTheRougarou

      requiredClueCount <- getPlayerCountValue (PerPlayer 4)
      learnedMoreAboutTheCurse <-
        (>= requiredClueCount)
          <$> getSpendableClueCount investigatorIds

      requiredDamage <- getPlayerCountValue (PerPlayer 1)
      protectedOurselves <-
        (>= requiredDamage) <$> field EnemyDamage rougarou

      assetIds <- selectList (EnemyAsset rougarou)
      keptItContained <- or <$> for assetIds (fieldMap AssetTraits (member Trap))

      scenarioLogs <- scenarioField ScenarioRemembered
      let
        calmedItDown =
          any
            (`member` scenarioLogs)
            [FoundAStrangeDoll, FoundAnAncientBindingStone]

      if and
        [ learnedMoreAboutTheCurse
        , keptItContained
        , protectedOurselves
        , calmedItDown
        ]
        then
          push
            ( chooseOne
                leadInvestigatorId
                [Label "Resolution 3" [ScenarioResolution $ Resolution 3]]
            )
        else
          push
            ( chooseOne
                leadInvestigatorId
                [Label "Flip back to a side" [RevertAct $ toId attrs]]
            )
      pure $ HuntingTheRougarou $ attrs & (sequenceL .~ Sequence 2 B)
    RevertAct aid
      | aid == toId attrs && onSide B attrs ->
          pure $ HuntingTheRougarou $ attrs & (sequenceL .~ Sequence 2 A)
    _ -> HuntingTheRougarou <$> runMessage msg attrs
