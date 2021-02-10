module Arkham.Types.Effect.Effects.BindMonster2
  ( bindMonster2
  , BindMonster2(..)
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
import Arkham.Types.Trait

newtype BindMonster2 = BindMonster2 EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bindMonster2 :: EffectArgs -> BindMonster2
bindMonster2 = BindMonster2 . uncurry4 (baseAttrs "02031")

instance HasModifiersFor env BindMonster2 where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasSet Trait env EnemyId) => RunMessage env BindMonster2 where
  runMessage msg e@(BindMonster2 attrs@EffectAttrs {..}) = case msg of
    PassedSkillTest _ (Just Action.Evade) _ (SkillTestInitiatorTarget (EnemyTarget eid)) _ _
      | SkillTestTarget == effectTarget
      -> case effectSource of
        (EventSource evid) -> do
          nonElite <- notMember Elite <$> getSet eid
          e <$ when
            nonElite
            (unshiftMessages
              [AttachEvent evid (EnemyTarget eid), DisableEffect effectId]
            )
        _ -> pure e
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect effectId)
    _ -> BindMonster2 <$> runMessage msg attrs
