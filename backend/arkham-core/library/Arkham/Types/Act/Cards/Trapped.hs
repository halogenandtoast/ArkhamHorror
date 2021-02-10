module Arkham.Types.Act.Cards.Trapped where

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

newtype Trapped = Trapped ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trapped :: Trapped
trapped = Trapped $ baseAttrs
  "01108"
  "Trapped"
  (Act 1 A)
  (Just $ RequiredClues (PerPlayer 2) Nothing)

instance ActionRunner env => HasActions env Trapped where
  getActions i window (Trapped x) = getActions i window x

instance ActRunner env => RunMessage env Trapped where
  runMessage msg a@(Trapped attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      requiredClues <- getPlayerCountValue (PerPlayer 2)
      unshiftMessages
        [ SpendClues requiredClues investigatorIds
        , chooseOne leadInvestigatorId [AdvanceAct aid $ toSource attrs]
        ]
      pure $ Trapped $ attrs & sequenceL .~ Act 1 B
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      enemyIds <- getSetList (LocationId "01111")
      a <$ unshiftMessages
        ([ PlaceLocation "01112"
         , PlaceLocation "01114"
         , PlaceLocation "01113"
         , PlaceLocation "01115"
         ]
        <> map (Discard . EnemyTarget) enemyIds
        <> [ RevealLocation Nothing "01112"
           , MoveAllTo "01112"
           , RemoveLocation "01111"
           , NextAct aid "01109"
           ]
        )
    _ -> Trapped <$> runMessage msg attrs
