module Arkham.Types.Effect.Effects.RiteOfSeeking
  ( riteOfSeeking
  , RiteOfSeeking(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Token
import qualified Arkham.Types.Action as Action
import Arkham.Types.Effect.Attrs

newtype RiteOfSeeking = RiteOfSeeking EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfSeeking :: EffectArgs -> RiteOfSeeking
riteOfSeeking = RiteOfSeeking . uncurry4 (baseAttrs "02028")

instance HasModifiersFor env RiteOfSeeking where
  getModifiersFor = noModifiersFor

instance (HasQueue env) => RunMessage env RiteOfSeeking where
  runMessage msg e@(RiteOfSeeking attrs@EffectAttrs {..}) = case msg of
    RevealToken _ iid token -> case effectTarget of
      InvestigationTarget iid' _ | iid == iid' -> e <$ when
        (token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
        (unshiftMessage $ CreateEffect
          "02028"
          Nothing
          (toSource attrs)
          (InvestigatorTarget iid)
        )
      _ -> pure e
    SkillTestEnds _ -> e <$ case effectTarget of
      InvestigatorTarget iid ->
        unshiftMessages [DisableEffect effectId, EndTurn iid]
      _ -> unshiftMessage (DisableEffect effectId)
    SuccessfulInvestigation iid _ source | isSource attrs source ->
      case effectTarget of
        InvestigationTarget _ lid' -> e <$ unshiftMessage
          (InvestigatorDiscoverClues iid lid' 1 (Just Action.Investigate))
        _ -> pure e
    _ -> RiteOfSeeking <$> runMessage msg attrs
