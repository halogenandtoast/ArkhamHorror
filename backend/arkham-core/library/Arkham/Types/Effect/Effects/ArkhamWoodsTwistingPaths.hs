module Arkham.Types.Effect.Effects.ArkhamWoodsTwistingPaths
  ( arkhamWoodsTwistingPaths
  , ArkhamWoodsTwistingPaths(..)
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

newtype ArkhamWoodsTwistingPaths = ArkhamWoodsTwistingPaths EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsTwistingPaths :: EffectArgs -> ArkhamWoodsTwistingPaths
arkhamWoodsTwistingPaths =
  ArkhamWoodsTwistingPaths . uncurry4 (baseAttrs "01151")

instance HasModifiersFor env ArkhamWoodsTwistingPaths where
  getModifiersFor = noModifiersFor

instance HasQueue env => RunMessage env ArkhamWoodsTwistingPaths where
  runMessage msg e@(ArkhamWoodsTwistingPaths attrs) = case msg of
    PassedSkillTest _ _ (LocationSource "01151") SkillTestInitiatorTarget{} _ _
      -> do
        let disable = DisableEffect (effectId attrs)
        e <$ case effectMetadata attrs of
          Just (EffectMessages msgs) -> unshiftMessages (msgs <> [disable])
          _ -> unshiftMessage disable
    FailedSkillTest _ _ (LocationSource "01151") SkillTestInitiatorTarget{} _ _
      -> e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> ArkhamWoodsTwistingPaths <$> runMessage msg attrs
