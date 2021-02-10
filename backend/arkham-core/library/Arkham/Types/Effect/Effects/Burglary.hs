module Arkham.Types.Effect.Effects.Burglary
  ( burglary
  , Burglary(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers

newtype Burglary = Burglary EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burglary :: EffectArgs -> Burglary
burglary = Burglary . uncurry4 (baseAttrs "01045")

instance HasModifiersFor env Burglary where
  getModifiersFor _ (LocationTarget lid) (Burglary attrs@EffectAttrs {..}) =
    case effectTarget of
      InvestigationTarget _ lid' | lid == lid' ->
        pure [toModifier attrs AlternateSuccessfullInvestigation]
      _ -> pure []
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env Burglary where
  runMessage msg e@(Burglary attrs@EffectAttrs {..}) = case msg of
    CreatedEffect eid _ _ (InvestigationTarget iid lid) | eid == effectId ->
      e <$ unshiftMessage
        (Investigate iid lid (toSource attrs) SkillIntellect False)
    SuccessfulInvestigation iid _ source | isSource attrs source ->
      e <$ unshiftMessages [TakeResources iid 3 False, DisableEffect effectId]
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect effectId)
    _ -> Burglary <$> runMessage msg attrs
