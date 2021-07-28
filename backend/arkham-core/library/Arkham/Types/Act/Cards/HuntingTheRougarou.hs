module Arkham.Types.Act.Cards.HuntingTheRougarou where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window

newtype HuntingTheRougarou = HuntingTheRougarou ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

huntingTheRougarou :: ActCard HuntingTheRougarou
huntingTheRougarou =
  act (2, A) HuntingTheRougarou Cards.huntingTheRougarou Nothing

ability :: ActAttrs -> Ability
ability attrs = (mkAbility (toSource attrs) 1 (FastAbility Free))
  { abilityLimit = PlayerLimit PerPhase 1
  }

instance ActionRunner env => HasActions env HuntingTheRougarou where
  getActions iid FastPlayerWindow (HuntingTheRougarou a) =
    withBaseActions iid FastPlayerWindow a $ do
      mrougarou <- fmap unStoryEnemyId <$> getId (CardCode "81028")
      engagedWithTheRougarou <- maybe
        (pure False)
        ((member iid <$>) . getSet)
        mrougarou
      pure [ UseAbility iid (ability a) | engagedWithTheRougarou ]
  getActions i window (HuntingTheRougarou x) = getActions i window x

instance ActRunner env => RunMessage env HuntingTheRougarou where
  runMessage msg a@(HuntingTheRougarou attrs@ActAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
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
            [Label "Flip back to a side" [RevertAct actId]]
          )
      pure $ HuntingTheRougarou $ attrs & (sequenceL .~ Act 2 B)
    RevertAct aid | aid == actId && onSide B attrs ->
      pure $ HuntingTheRougarou $ attrs & (sequenceL .~ Act 2 A)
    EnemyMove eid lid _ -> do
      isRougarou <- (== CardCode "81028") <$> getId eid
      a <$ when isRougarou (push (PlaceClues (LocationTarget lid) 1))
    EnemyDefeated _ _ _ "81028" _ _ ->
      a <$ push (ScenarioResolution $ Resolution 2)
    _ -> HuntingTheRougarou <$> runMessage msg attrs
