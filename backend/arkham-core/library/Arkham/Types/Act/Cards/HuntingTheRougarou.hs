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
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyDefeated)
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as Window

newtype HuntingTheRougarou = HuntingTheRougarou ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingTheRougarou :: ActCard HuntingTheRougarou
huntingTheRougarou =
  act (2, A) HuntingTheRougarou Cards.huntingTheRougarou Nothing

instance HasAbilities HuntingTheRougarou where
  getAbilities (HuntingTheRougarou a) =
    [ mkAbility a 1
    $ ForcedAbility
    $ EnemyLeaves Timing.After Anywhere
    $ enemyIs Cards.theRougarou
    , mkAbility a 2 $ ForcedAbility $ EnemyDefeated Timing.When Anyone $ enemyIs
      Cards.theRougarou
    , restrictedAbility
        a
        3
        (InvestigatorExists $ You <> InvestigatorEngagedWith
          (enemyIs Cards.theRougarou)
        )
        (Objective $ FastAbility Free)
      & (abilityLimitL .~ PlayerLimit PerPhase 1)
    ]

instance ActRunner env => RunMessage env HuntingTheRougarou where
  runMessage msg a@(HuntingTheRougarou attrs) = case msg of
    UseCardAbility _ source [Window _ (Window.EnemyLeaves _ lid)] 1 _
      | isSource attrs source -> a <$ push (PlaceClues (LocationTarget lid) 1)
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      a <$ push (ScenarioResolution $ Resolution 2)
    UseCardAbility _ source _ 3 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source)
    AdvanceAct aid _ | aid == toId attrs && onSide A attrs -> do
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

      assetIds <- selectList (EnemyAsset rougarou)
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
            [Label "Flip back to a side" [RevertAct $ toId attrs]]
          )
      pure $ HuntingTheRougarou $ attrs & (sequenceL .~ Act 2 B)
    RevertAct aid | aid == toId attrs && onSide B attrs ->
      pure $ HuntingTheRougarou $ attrs & (sequenceL .~ Act 2 A)
    _ -> HuntingTheRougarou <$> runMessage msg attrs
