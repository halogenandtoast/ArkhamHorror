module Arkham.Effect.Effects.OnNextTurnEffect (
  OnNextTurnEffect (..),
  onNextTurnEffect,
  onNextTurnEffect',
) where

import Arkham.Classes
import Arkham.Effect.Runner hiding (onNextTurnEffect)
import Arkham.Prelude

newtype OnNextTurnEffect = OnNextTurnEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onNextTurnEffect :: EffectArgs -> OnNextTurnEffect
onNextTurnEffect = OnNextTurnEffect . uncurry (baseAttrs "onsuc")

onNextTurnEffect'
  :: EffectId
  -> Source
  -> InvestigatorId
  -> [Message]
  -> OnNextTurnEffect
onNextTurnEffect' eid source iid msgs =
  OnNextTurnEffect
    $ EffectAttrs
      { effectId = eid
      , effectSource = source
      , effectTarget = InvestigatorTarget iid
      , effectCardCode = "onnex"
      , effectMetadata = Just (EffectMessages msgs)
      , effectTraits = mempty
      , effectWindow = Nothing
      , effectDisableWindow = Nothing
      , effectOnDisable = Nothing
      , effectFinished = False
      , effectExtraMetadata = Null
      , effectSkillTest = Nothing
      , effectCardId = Nothing
      , effectMetaKeys = []
      }

instance HasModifiersFor OnNextTurnEffect

instance RunMessage OnNextTurnEffect where
  runMessage msg e@(OnNextTurnEffect attrs) = runQueueT $ case msg of
    BeginTurn iid | InvestigatorTarget iid == attrs.target -> do
      case attrs.metadata of
        Just (EffectMessages msgs) -> do
          push $ DisableEffect attrs.id
          pushAll msgs
        _ -> pure ()
      pure e
    _ -> OnNextTurnEffect <$> liftRunMessage msg attrs
