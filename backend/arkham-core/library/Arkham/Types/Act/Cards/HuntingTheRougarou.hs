module Arkham.Types.Act.Cards.HuntingTheRougarou where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Trait

newtype HuntingTheRougarou = HuntingTheRougarou ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingTheRougarou :: HuntingTheRougarou
huntingTheRougarou = HuntingTheRougarou
  $ baseAttrs "81006" "Hunting the Rougarou" (Act 2 A) Nothing

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
      pure
        [ ActivateCardAbilityAction iid (ability a) | engagedWithTheRougarou ]
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

      assetIds <- getSetList @AssetId rougarou
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
        then unshiftMessage
          (chooseOne
            leadInvestigatorId
            [Label "Resolution 3" [ScenarioResolution $ Resolution 3]]
          )
        else unshiftMessage
          (chooseOne
            leadInvestigatorId
            [Label "Flip back to a side" [RevertAct actId]]
          )
      pure $ HuntingTheRougarou $ attrs & (sequenceL .~ Act 2 B)
    RevertAct aid | aid == actId && onSide B attrs ->
      pure $ HuntingTheRougarou $ attrs & (sequenceL .~ Act 2 A)
    EnemyMove eid lid _ -> do
      isRougarou <- (== CardCode "81028") <$> getId eid
      a <$ when isRougarou (unshiftMessage (PlaceClues (LocationTarget lid) 1))
    EnemyDefeated _ _ _ "81028" _ _ ->
      a <$ unshiftMessage (ScenarioResolution $ Resolution 2)
    _ -> HuntingTheRougarou <$> runMessage msg attrs
