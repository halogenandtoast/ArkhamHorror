module Arkham.Event.Cards.Snipe1 (snipe1, snipe1Effect, Snipe1 (..)) where

import Arkham.Action (Action (Fight))
import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestSource)
import Arkham.Matcher

newtype Snipe1 = Snipe1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snipe1 :: EventCard Snipe1
snipe1 = event Snipe1 Cards.snipe1

instance RunMessage Snipe1 where
  runMessage msg e@(Snipe1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      createCardEffect Cards.snipe1 Nothing attrs iid
      pure e
    _ -> Snipe1 <$> liftRunMessage msg attrs

newtype Snipe1Effect = Snipe1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snipe1Effect :: EffectArgs -> Snipe1Effect
snipe1Effect = cardEffect Snipe1Effect Cards.snipe1

instance HasModifiersFor Snipe1Effect where
  getModifiersFor (ChaosTokenTarget t) (Snipe1Effect attrs)
    | t.face `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail] = maybeModified attrs do
        Fight <- MaybeT getSkillTestAction
        aid <- MaybeT $ join . fmap (.asset) <$> getSkillTestSource
        guardM $ lift $ aid <=~> oneOf @AssetMatcher [#firearm, #ranged]
        pure [ChaosTokenFaceModifier [Zero]]
  getModifiersFor _ _ = pure []

instance RunMessage Snipe1Effect where
  runMessage msg e@(Snipe1Effect attrs) = runQueueT $ case msg of
    EndTurn iid | attrs.target == toTarget iid -> disableReturn e
    SkillTestEnds iid (AssetSource aid) | attrs.target == toTarget iid -> do
      whenM (aid <=~> oneOf @AssetMatcher [#firearm, #ranged]) do
        mAction <- getSkillTestAction
        when (mAction == Just #fight) (disable attrs)
      pure e
    _ -> Snipe1Effect <$> runMessage msg attrs
