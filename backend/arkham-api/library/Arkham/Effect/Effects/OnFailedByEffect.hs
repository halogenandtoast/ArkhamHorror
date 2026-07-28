module Arkham.Effect.Effects.OnFailedByEffect (
  OnFailedByEffect (..),
  onFailedByEffect,
  onFailedByEffect',
) where

import Arkham.Classes
import Arkham.Effect.Runner hiding (onFailedByEffect)
import Arkham.Helpers.GameValue (gameValueMatches)
import Arkham.Matcher hiding (RevealChaosToken, SkillTestEnded)
import Arkham.Prelude

newtype OnFailedByEffect = OnFailedByEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onFailedByEffect :: EffectArgs -> OnFailedByEffect
onFailedByEffect = OnFailedByEffect . uncurry (baseAttrs "onfal")

onFailedByEffect'
  :: EffectId
  -> SkillTestId
  -> ValueMatcher
  -> Source
  -> Target
  -> [Message]
  -> OnFailedByEffect
onFailedByEffect' eid skillTestId matchr source target msgs =
  OnFailedByEffect
    $ EffectAttrs
      { effectId = eid
      , effectSource = source
      , effectTarget = target
      , effectCardCode = "onfal"
      , effectMetadata = Just (EffectMessages msgs)
      , effectTraits = mempty
      , effectWindow = Nothing
      , effectDisableWindow = Nothing
      , effectOnDisable = Nothing
      , effectFinished = False
      , effectExtraMetadata = toJSON matchr
      , effectSkillTest = Just skillTestId
      , effectCardId = Nothing
      , effectMetaKeys = []
      }

instance HasModifiersFor OnFailedByEffect

instance RunMessage OnFailedByEffect where
  runMessage msg e@(OnFailedByEffect attrs) = runQueueT $ case msg of
    FailedThisSkillTestBy _ _ n -> do
      void $ runMaybeT do
        sid <- MaybeT getSkillTestId
        guard (sid `elem` effectSkillTest attrs)
        matchr <- hoistMaybe $ maybeResult $ effectExtraMetadata attrs
        liftGuardM $ gameValueMatches n matchr
        case attrs.metadata of
          Just (EffectMessages msgs) -> lift do
            push $ DisableEffect attrs.id
            pushAll msgs
          _ -> pure ()
      pure e
    -- see: Arkham.Effect.Effects.OnSucceedByEffect, the rider follows a
    -- repeated skill test and is only disabled once the test truly ends
    RepeatSkillTest sid stId | Just stId == attrs.skillTest -> do
      pure . OnFailedByEffect $ attrs {effectSkillTest = Just sid}
    SkillTestEnded sid | Just sid == attrs.skillTest -> do
      push $ DisableEffect attrs.id
      pure e
    _ -> OnFailedByEffect <$> liftRunMessage msg attrs
