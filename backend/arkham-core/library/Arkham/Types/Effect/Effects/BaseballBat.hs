module Arkham.Types.Effect.Effects.BaseballBat
  ( baseballBat
  , BaseballBat(..)
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


import Arkham.Types.Effect.Attrs

newtype BaseballBat = BaseballBat EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseballBat :: EffectArgs -> BaseballBat
baseballBat = BaseballBat . uncurry4 (baseAttrs "01074")

instance HasModifiersFor env BaseballBat where
  getModifiersFor = noModifiersFor

instance HasQueue env => RunMessage env BaseballBat where
  runMessage msg e@(BaseballBat attrs@EffectAttrs {..}) = case msg of
    RevealToken _ iid token | InvestigatorTarget iid == effectTarget ->
      case effectSource of
        AssetSource assetId -> e <$ when
          (token `elem` [Skull, AutoFail])
          (unshiftMessages
            [Discard (AssetTarget assetId), DisableEffect effectId]
          )
        _ -> error "wrong source"
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect effectId)
    _ -> BaseballBat <$> runMessage msg attrs
