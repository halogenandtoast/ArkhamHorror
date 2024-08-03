module Arkham.Event.Cards.DawnStar1 (dawnStar1, dawnStar1Effect, DawnStar1 (..)) where

import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens, withSkillTest)
import Arkham.Matcher hiding (RevealChaosToken, SkillTestEnded)

newtype DawnStar1 = DawnStar1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dawnStar1 :: EventCard DawnStar1
dawnStar1 = event DawnStar1 Cards.dawnStar1

instance RunMessage DawnStar1 where
  runMessage msg e@(DawnStar1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      withSkillTest $ createCardEffect Cards.dawnStar1 (effectMetaTarget iid) attrs
      pure e
    _ -> DawnStar1 <$> liftRunMessage msg attrs

newtype DawnStar1Effect = DawnStar1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dawnStar1Effect :: EffectArgs -> DawnStar1Effect
dawnStar1Effect = cardEffect DawnStar1Effect Cards.dawnStar1

instance HasModifiersFor DawnStar1Effect where
  getModifiersFor (ChaosTokenTarget (chaosTokenFace -> CurseToken)) (DawnStar1Effect attrs) = do
    modified attrs [IgnoreChaosTokenModifier]
  getModifiersFor _ _ = pure []

instance RunMessage DawnStar1Effect where
  runMessage msg e@(DawnStar1Effect attrs) = runQueueT $ case msg of
    CreatedEffect eid _ _ _ | eid == attrs.id -> do
      tokens <- count ((== #curse) . (.face)) <$> getSkillTestRevealedChaosTokens
      doStep tokens msg
      pure e
    SkillTestEnded sid | isTarget sid attrs.target -> do
      disableReturn e
    DoStep n msg'@(CreatedEffect eid _ _ _) | eid == attrs.id && n > 0 -> do
      for_ attrs.metaTarget \case
        InvestigatorTarget iid ->
          selectOneToHandleWith iid attrs (DoStep (n - 1) msg')
            $ enemyAtLocationWith iid
            <> EnemyCanBeDamagedBySource (toSource attrs)
        _ -> pure ()
      pure e
    After (RevealChaosToken _ _ token) | token.face == #curse -> do
      for_ attrs.metaTarget \case
        InvestigatorTarget iid ->
          selectOneToHandle iid attrs
            $ enemyAtLocationWith iid
            <> EnemyCanBeDamagedBySource (toSource attrs)
        _ -> pure ()
      pure e
    HandleTargetChoice _iid (isSource attrs -> True) (EnemyTarget eid) -> do
      nonAttackEnemyDamage attrs 1 eid
      pure e
    _ -> DawnStar1Effect <$> liftRunMessage msg attrs
