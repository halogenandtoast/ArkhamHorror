module Arkham.Event.Cards.ThirdTimesACharm2 (thirdTimesACharm2, thirdTimesACharm2Effect, ThirdTimesACharm2 (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Effect.Runner hiding (RevealChaosToken)
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner hiding (RevealChaosToken)
import Arkham.Game.Helpers
import Arkham.Matcher hiding (SkillTestEnded)
import Arkham.Prelude
import Arkham.Window (revealedChaosTokens)

newtype ThirdTimesACharm2 = ThirdTimesACharm2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thirdTimesACharm2 :: EventCard ThirdTimesACharm2
thirdTimesACharm2 = event ThirdTimesACharm2 Cards.thirdTimesACharm2

instance RunMessage ThirdTimesACharm2 where
  runMessage msg e@(ThirdTimesACharm2 attrs) = case msg of
    PlayThisEvent _iid eid | eid == toId attrs -> do
      withSkillTest \sid ->
        push $ createCardEffect Cards.thirdTimesACharm2 (Just $ EffectInt 2) attrs sid
      pure e
    _ -> ThirdTimesACharm2 <$> runMessage msg attrs

newtype ThirdTimesACharm2Effect = ThirdTimesACharm2Effect EffectAttrs
  deriving anyclass (HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thirdTimesACharm2Effect :: EffectArgs -> ThirdTimesACharm2Effect
thirdTimesACharm2Effect = cardEffect ThirdTimesACharm2Effect Cards.thirdTimesACharm2

-- TODO: technically the limit here should be enough so that we don't need to
-- rely on the EffectMetadata, but it didn't seem to be working
instance HasAbilities ThirdTimesACharm2Effect where
  getAbilities (ThirdTimesACharm2Effect a) = case a.meta of
    Just (EffectInt n) -> case a.target of
      SkillTestTarget sid ->
        [ withTooltip "Cancel it, return it to the chaos bag, and reveal a new chaos token"
          $ limitedAbility (GroupLimit PerTestOrAbility 2)
          $ mkAbility (proxied (SkillTestSource sid) a) 1
          $ freeReaction
          $ RevealChaosToken #when You #any
        | n > 0
        ]
      _ -> error "incorrect source"
    _ -> []

instance RunMessage ThirdTimesACharm2Effect where
  runMessage msg e@(ThirdTimesACharm2Effect attrs) = case msg of
    UseCardAbility iid (isProxySource attrs -> True) 1 (revealedChaosTokens -> [token]) _ -> do
      case attrs.meta of
        Just (EffectInt n) -> do
          cancelChaosToken token
          pushAll
            [ CancelEachNext attrs.source [RunWindowMessage, DrawChaosTokenMessage, RevealChaosTokenMessage]
            , ReturnChaosTokens [token]
            , UnfocusChaosTokens
            , DrawAnotherChaosToken iid
            ]
          pure $ ThirdTimesACharm2Effect $ attrs & metadataL ?~ EffectInt (n - 1)
        _ -> error "wrong meta"
    SkillTestEnded {} -> do
      push $ disable attrs
      pure e
    _ -> ThirdTimesACharm2Effect <$> runMessage msg attrs
