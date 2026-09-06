module Arkham.Effect.Effects.OnRevealChaosTokenEffect (
  OnRevealChaosTokenEffect (..),
  onRevealChaosTokenEffect,
  onRevealChaosTokenEffect',
) where

import Arkham.Card (toCardId)
import Arkham.ChaosToken (ChaosToken)
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Helpers.Modifiers (getModifiers)
import Arkham.Helpers.Ref (sourceToMaybeCard, sourceToTarget)
import Arkham.Matcher hiding (RevealChaosToken, SkillTestEnded)
import Arkham.Message.Lifted.Queue
import Arkham.Modifier (ModifierType (ResolveEffectsAdditionalTimes))
import Arkham.Prelude
import Arkham.Window qualified as Window

newtype OnRevealChaosTokenEffect = OnRevealChaosTokenEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onRevealChaosTokenEffect :: EffectArgs -> OnRevealChaosTokenEffect
onRevealChaosTokenEffect = OnRevealChaosTokenEffect . uncurry (baseAttrs "ontok")

onRevealChaosTokenEffect'
  :: EffectId
  -> SkillTestId
  -> ChaosTokenMatcher
  -> Source
  -> Target
  -> [Message]
  -> OnRevealChaosTokenEffect
onRevealChaosTokenEffect' eid skillTestId matchr source target msgs =
  OnRevealChaosTokenEffect
    $ EffectAttrs
      { effectId = eid
      , effectSource = source
      , effectTarget = target
      , effectCardCode = "ontok"
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

instance HasModifiersFor OnRevealChaosTokenEffect

handleToken :: ReverseQueue m => EffectAttrs -> InvestigatorId -> ChaosToken -> m ()
handleToken attrs iid token = void $ runMaybeT do
  matchr :: ChaosTokenMatcher <- hoistMaybe $ maybeResult $ effectExtraMetadata attrs
  liftGuardM $ matches token (IncludeSealed matchr)
  sid <- MaybeT getSkillTestId
  guard $ Just sid == effectSkillTest attrs
  case attrs.metadata of
    Just (EffectMessages msgs) -> lift do
      push $ DisableEffect attrs.id
      -- "Resolve that card's effects an additional time" is the card being told
      -- about the same token more than once, so the count is decided here where
      -- the telling happens. Read off the card as well as the entity: the
      -- choice is made against a committed card, which is all the chooser has
      -- to point at.
      mods <- getModifiers (sourceToTarget attrs.source)
      cardMods <- maybe (pure []) (getModifiers . toCardId) =<< sourceToMaybeCard attrs.source
      let extra = sum $ map extraTimes (mods <> cardMods)
          times = 1 + max 0 extra
          repeat_ m = pushAll (replicate times m)
      case attrs.source of
        EventSource eid -> repeat_ $ If (Window.RevealChaosTokenEventEffect iid [token] eid) msgs
        AbilitySource inner _n -> case inner of
          AssetSource aid -> repeat_ $ If (Window.RevealChaosTokenAssetAbilityEffect iid [token] aid) msgs
          other -> error $ "Unhandled ability source for token effect: " <> show other
        UseAbilitySource _ inner _n -> case inner of
          AssetSource aid -> repeat_ $ If (Window.RevealChaosTokenAssetAbilityEffect iid [token] aid) msgs
          other -> error $ "Unhandled ability source for token effect: " <> show other
        AssetSource aid -> repeat_ $ If (Window.RevealChaosTokenAssetAbilityEffect iid [token] aid) msgs
        SkillSource skid -> repeat_ $ If (Window.RevealChaosTokenSkillEffect iid [token] skid) msgs
        TreacherySource tid -> repeat_ $ If (Window.RevealChaosTokenTreacheryEffect iid [token] tid) msgs
        ChaosTokenEffectSource _ -> pushAll (mconcat (replicate times msgs))
        LocationSource _ -> pushAll (mconcat (replicate times msgs))
        other -> error $ "Unhandled source for token effect: " <> show other
    _ -> pure ()
 where
  extraTimes = \case
    ResolveEffectsAdditionalTimes n -> n
    _ -> 0

instance RunMessage OnRevealChaosTokenEffect where
  runMessage msg e@(OnRevealChaosTokenEffect attrs) = runQueueT $ case msg of
    RevealChaosToken _ iid token -> do
      handleToken attrs iid token
      pure e
    SilentRevealChaosToken _ iid token -> do
      handleToken attrs iid token
      pure e
    -- see: Arkham.Effect.Effects.OnSucceedByEffect, the rider follows a
    -- repeated skill test and is only disabled once the test truly ends
    RepeatSkillTest sid stId | Just stId == attrs.skillTest -> do
      pure . OnRevealChaosTokenEffect $ attrs {effectSkillTest = Just sid}
    SkillTestEnded sid | Just sid == attrs.skillTest -> do
      push $ DisableEffect attrs.id
      pure e
    _ -> OnRevealChaosTokenEffect <$> liftRunMessage msg attrs
