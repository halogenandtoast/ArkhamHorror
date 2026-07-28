module Arkham.Effect.Effects.OnSucceedByEffect (
  OnSucceedByEffect (..),
  onSucceedByEffect,
  onSucceedByEffect',
) where

import Arkham.Classes
import Arkham.Effect.Runner hiding (onSucceedByEffect)
import Arkham.Helpers.GameValue (gameValueMatches)
import Arkham.Helpers.Ref (sourceToMaybeCard)
import Arkham.Matcher hiding (RevealChaosToken, SkillTestEnded)
import Arkham.Message.Lifted (skillTestCardOption)
import Arkham.Prelude

newtype OnSucceedByEffect = OnSucceedByEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onSucceedByEffect :: EffectArgs -> OnSucceedByEffect
onSucceedByEffect = OnSucceedByEffect . uncurry (baseAttrs "onsuc")

onSucceedByEffect'
  :: EffectId
  -> SkillTestId
  -> ValueMatcher
  -> Source
  -> Target
  -> [Message]
  -> OnSucceedByEffect
onSucceedByEffect' eid skillTestId matchr source target msgs =
  OnSucceedByEffect
    $ EffectAttrs
      { effectId = eid
      , effectSource = source
      , effectTarget = target
      , effectCardCode = "onsuc"
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

instance HasModifiersFor OnSucceedByEffect

instance RunMessage OnSucceedByEffect where
  runMessage msg e@(OnSucceedByEffect attrs) = runQueueT $ case msg of
    PassedThisSkillTestBy _ _ n -> do
      void $ runMaybeT do
        sid <- MaybeT getSkillTestId
        guard (sid `elem` effectSkillTest attrs)
        matchr <- hoistMaybe $ maybeResult $ effectExtraMetadata attrs
        liftGuardM $ gameValueMatches n matchr
        case attrs.metadata of
          Just (EffectMessages msgs) -> lift do
            push $ DisableEffect attrs.id
            -- Register the on-success effect as a skill-test option so the player
            -- can order it relative to other on-success effects (e.g. a treachery
            -- that discards itself when its own test succeeds) instead of it
            -- always resolving first.
            sourceToMaybeCard attrs.source >>= \case
              Just card -> skillTestCardOption card $ pushAll msgs
              Nothing -> pushAll msgs
          _ -> pure ()
      pure e
    -- The rider follows the test it is attached to when that test is repeated
    -- (Live and Learn, Daniel Jameson, ...), otherwise Act of Desperation's
    -- "gain resources" would be dropped before the repeat is even declared.
    RepeatSkillTest sid stId | Just stId == attrs.skillTest -> do
      pure . OnSucceedByEffect $ attrs {effectSkillTest = Just sid}
    -- Disable at SkillTestEnded (ST.8, after the "skill test ended" window)
    -- rather than SkillTestEnds, which fires before that window and so before
    -- a repeat can be declared.
    SkillTestEnded sid | Just sid == attrs.skillTest -> do
      push $ DisableEffect attrs.id
      pure e
    _ -> OnSucceedByEffect <$> liftRunMessage msg attrs
