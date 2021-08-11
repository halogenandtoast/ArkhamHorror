module Arkham.Types.Act.Cards.HuntingTheRougarou where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Matcher (enemyIs)
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Resolution
import qualified Arkham.Types.Restriction as R
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait
import qualified Arkham.Types.Window as W

newtype HuntingTheRougarou = HuntingTheRougarou ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

huntingTheRougarou :: ActCard HuntingTheRougarou
huntingTheRougarou =
  act (2, A) HuntingTheRougarou Cards.huntingTheRougarou Nothing

instance HasActions HuntingTheRougarou where
  getActions (HuntingTheRougarou x) =
    [ mkAbility x 1
      $ ForcedAbility
      $ R.EnemyLeaves Timing.After R.Anywhere
      $ enemyIs Cards.theRougarou
    , mkAbility x 2
      $ Objective
      $ ForcedAbility
      $ R.EnemyDefeated Timing.When R.Anyone
      $ enemyIs Cards.theRougarou
    , restrictedAbility
        x
        3
        (R.InvestigatorExists $ R.You <> R.InvestigatorEngagedWith
          (enemyIs Cards.theRougarou)
        )
        (Objective $ FastAbility Free)
      & abilityLimitL
      .~ PlayerLimit PerPhase 1
    ]

instance ActRunner env => RunMessage env HuntingTheRougarou where
  runMessage msg a@(HuntingTheRougarou attrs@ActAttrs {..}) = case msg of
    UseCardAbility _ source [W.Window _ (W.EnemyLeaves _ lid)] 1 _
      | isSource attrs source -> a <$ push (PlaceClues (LocationTarget lid) 1)
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      a <$ push (ScenarioResolution $ Resolution 2)
    UseCardAbility _ source _ 3 _ | isSource attrs source ->
      runMessage (AdvanceAct actId (toSource attrs)) a
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      rougarou <- unStoryEnemyId . fromJustNote "must be" <$> getId
        (CardCode "81028")

      requiredClueCount <- getPlayerCountValue (PerPlayer 4)
      learnedMoreAboutTheCurse <- (>= requiredClueCount)
        <$> getSpendableClueCount investigatorIds

      requiredDamage <- getPlayerCountValue (PerPlayer 1)
      protectedOurselves <-
        (>= requiredDamage) . unDamageCount <$> getCount rougarou

      assetIds <- selectList (R.EnemyAsset rougarou)
      keptItContained <- or <$> for assetIds ((member Trap <$>) . getSet)

      scenarioLogs <- getSet ()
      let
        calmedItDown = any
          (`member` scenarioLogs)
          [FoundAStrangeDoll, FoundAnAncientBindingStone]

      if and
          [ learnedMoreAboutTheCurse
          , keptItContained
          , protectedOurselves
          , calmedItDown
          ]
        then push
          (chooseOne
            leadInvestigatorId
            [Label "Resolution 3" [ScenarioResolution $ Resolution 3]]
          )
        else push
          (chooseOne
            leadInvestigatorId
            [Label "Flip back to a side" [RevertAct actId]]
          )
      pure $ HuntingTheRougarou $ attrs & (sequenceL .~ Act 2 B)
    RevertAct aid | aid == actId && onSide B attrs ->
      pure $ HuntingTheRougarou $ attrs & (sequenceL .~ Act 2 A)
    _ -> HuntingTheRougarou <$> runMessage msg attrs
