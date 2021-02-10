module Arkham.Types.Act.Cards.IntoTheDarkness where

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
import Arkham.Types.Card.EncounterCardMatcher

newtype IntoTheDarkness = IntoTheDarkness ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheDarkness :: IntoTheDarkness
intoTheDarkness =
  IntoTheDarkness $ baseAttrs "01147" "Into the Darkness" (Act 2 A) Nothing

instance ActionRunner env => HasActions env IntoTheDarkness where
  getActions i window (IntoTheDarkness x) = getActions i window x

instance ActRunner env => RunMessage env IntoTheDarkness where
  runMessage msg a@(IntoTheDarkness attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage
        (chooseOne leadInvestigatorId [AdvanceAct aid (toSource attrs)])
      pure $ IntoTheDarkness $ attrs & (sequenceL .~ Act 2 B)
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      playerCount <- getPlayerCount
      if playerCount > 3
        then a <$ unshiftMessages
          [ ShuffleEncounterDiscardBackIn
          , DiscardEncounterUntilFirst
            (ActSource actId)
            (EncounterCardMatchByType (EnemyType, Nothing))
          , DiscardEncounterUntilFirst
            (ActSource actId)
            (EncounterCardMatchByType (EnemyType, Nothing))
          , NextAct actId "01148"
          ]
        else a <$ unshiftMessages
          [ ShuffleEncounterDiscardBackIn
          , DiscardEncounterUntilFirst
            (ActSource actId)
            (EncounterCardMatchByType (EnemyType, Nothing))
          , NextAct actId "01148"
          ]
    RequestedEncounterCard (ActSource aid) mcard | aid == actId -> case mcard of
      Nothing -> pure a
      Just card ->
        a <$ unshiftMessages [SpawnEnemyAt (EncounterCard card) "01156"]
    WhenEnterLocation _ "01156" ->
      a <$ unshiftMessage (AdvanceAct actId (toSource attrs))
    _ -> IntoTheDarkness <$> runMessage msg attrs
