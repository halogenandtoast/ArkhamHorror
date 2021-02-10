module Arkham.Types.Act.Cards.SearchingForTheTome
  ( SearchingForTheTome(..)
  , searchingForTheTome
  )
where

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

newtype SearchingForTheTome = SearchingForTheTome ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchingForTheTome :: SearchingForTheTome
searchingForTheTome = SearchingForTheTome
  $ baseAttrs "02125" "Searching for the Tome" (Act 3 A) Nothing

instance ActionRunner env => HasActions env SearchingForTheTome where
  getActions i window (SearchingForTheTome x) = do
    mRestrictedHall <- getId @(Maybe LocationId)
      (LocationWithFullTitle "Exhibit Hall" "Restricted Hall")
    case mRestrictedHall of
      Just restrictedHall -> do
        mustAdvance <- (== 0) . unClueCount <$> getCount restrictedHall
        if mustAdvance
          then pure [Force $ AdvanceAct (actId x) (toSource x)]
          else getActions i window x
      Nothing -> getActions i window x

instance ActRunner env => RunMessage env SearchingForTheTome where
  runMessage msg a@(SearchingForTheTome attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      unshiftMessage (AdvanceAct aid $ toSource attrs)
      pure . SearchingForTheTome $ attrs & sequenceL .~ Act 3 B
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ unshiftMessage
        (chooseOne
          leadInvestigatorId
          [ Label
            "It's too dangerous to keep around. We have to destroy it. (-> R1)"
            [ScenarioResolution $ Resolution 1]
          , Label
            "It's too valuable to destroy. We have to keep it safe. (-> R2)"
            [ScenarioResolution $ Resolution 2]
          ]
        )
    _ -> SearchingForTheTome <$> runMessage msg attrs
