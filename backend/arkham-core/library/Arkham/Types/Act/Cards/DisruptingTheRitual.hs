module Arkham.Types.Act.Cards.DisruptingTheRitual where

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

newtype DisruptingTheRitual = DisruptingTheRitual ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disruptingTheRitual :: DisruptingTheRitual
disruptingTheRitual =
  DisruptingTheRitual
    $ (baseAttrs "01148" "Disrupting the Ritual" (Act 3 A) Nothing)
        { actClues = Just 0
        }

instance ActionRunner env => HasActions env DisruptingTheRitual where
  getActions iid NonFast (DisruptingTheRitual a@ActAttrs {..}) = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility
          (toSource a)
          1
          (ActionAbility Nothing $ Costs [ActionCost 1, ClueCost 1])
        )
    ]
  getActions i window (DisruptingTheRitual x) = getActions i window x

instance ActRunner env => RunMessage env DisruptingTheRitual where
  runMessage msg a@(DisruptingTheRitual attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage
        (chooseOne leadInvestigatorId [AdvanceAct actId (toSource attrs)])
      pure $ DisruptingTheRitual $ attrs & (sequenceL .~ Act 3 B)
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ unshiftMessage (ScenarioResolution $ Resolution 1)
    PlaceClues (ActTarget aid) n | aid == actId -> do
      requiredClues <- getPlayerCountValue (PerPlayer 2)
      let totalClues = n + fromJustNote "Must be set" actClues
      when
        (totalClues >= requiredClues)
        (unshiftMessage (AdvanceAct actId $ toSource attrs))
      pure $ DisruptingTheRitual (attrs { actClues = Just totalClues })
    UseCardAbility iid (ActSource aid) _ 1 _ | aid == actId ->
      a <$ unshiftMessage
        (chooseOne
          iid
          [ BeginSkillTest
            iid
            (ActSource actId)
            (ActTarget actId)
            Nothing
            SkillWillpower
            3
          , BeginSkillTest
            iid
            (ActSource actId)
            (ActTarget actId)
            Nothing
            SkillAgility
            3
          ]
        )
    PassedSkillTest _ _ source _ _ _ | isSource attrs source ->
      a <$ unshiftMessage (PlaceClues (toTarget attrs) 1)
    _ -> DisruptingTheRitual <$> runMessage msg attrs
