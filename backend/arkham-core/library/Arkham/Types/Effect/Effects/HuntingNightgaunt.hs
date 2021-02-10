module Arkham.Types.Effect.Effects.HuntingNightgaunt
  ( huntingNightgaunt
  , HuntingNightgaunt(..)
  ) where

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


import Arkham.Types.Action
import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers

newtype HuntingNightgaunt = HuntingNightgaunt EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingNightgaunt :: EffectArgs -> HuntingNightgaunt
huntingNightgaunt = HuntingNightgaunt . uncurry4 (baseAttrs "01172")

instance HasModifiersFor env HuntingNightgaunt where
  getModifiersFor (SkillTestSource _ _ _ (Just Evade)) (DrawnTokenTarget _) (HuntingNightgaunt a@EffectAttrs {..})
    = pure $ toModifiers a [DoubleNegativeModifiersOnTokens]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env HuntingNightgaunt where
  runMessage msg e@(HuntingNightgaunt attrs) = case msg of
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> HuntingNightgaunt <$> runMessage msg attrs
