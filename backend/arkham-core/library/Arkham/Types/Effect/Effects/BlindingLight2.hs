module Arkham.Types.Effect.Effects.BlindingLight2
  ( blindingLight2
  , BlindingLight2(..)
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


import qualified Arkham.Types.Action as Action
import Arkham.Types.Effect.Attrs

newtype BlindingLight2 = BlindingLight2 EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blindingLight2 :: EffectArgs -> BlindingLight2
blindingLight2 = BlindingLight2 . uncurry4 (baseAttrs "01069")

instance HasModifiersFor env BlindingLight2 where
  getModifiersFor = noModifiersFor

instance HasQueue env => RunMessage env BlindingLight2 where
  runMessage msg e@(BlindingLight2 attrs@EffectAttrs {..}) = case msg of
    RevealToken _ iid token | InvestigatorTarget iid == effectTarget ->
      e <$ when
        (token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
        (unshiftMessages
          [ LoseActions iid (toSource attrs) 1
          , InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
          , DisableEffect effectId
          ]
        )
    PassedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget (EnemyTarget eid)) _ _
      | SkillTestTarget == effectTarget
      -> e <$ unshiftMessages
        [EnemyDamage eid iid (InvestigatorSource iid) 2, DisableEffect effectId]
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect effectId)
    _ -> BlindingLight2 <$> runMessage msg attrs
