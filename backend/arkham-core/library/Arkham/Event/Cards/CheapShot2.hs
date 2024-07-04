module Arkham.Event.Cards.CheapShot2 (cheapShot2, cheapShot2Effect, CheapShot2 (..)) where

import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.SkillTest (getSkillTestTarget)
import Arkham.Modifier

newtype CheapShot2 = CheapShot2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cheapShot2 :: EventCard CheapShot2
cheapShot2 = event CheapShot2 Cards.cheapShot2

instance RunMessage CheapShot2 where
  runMessage msg e@(CheapShot2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      skillTestModifier attrs iid (AddSkillValue #agility)
      pushM $ mkChooseFight iid attrs
      pure e
    PassedThisSkillTestBy iid (isSource attrs -> True) n | n >= 1 -> do
      when (n >= 3) $ createCardEffect Cards.cheapShot2 (effectMetaTarget attrs.cardId) attrs iid
      getSkillTestTarget >>= \case
        Just (EnemyTarget eid) -> push $ EnemyEvaded iid eid
        _ -> pure ()
      pure e
    _ -> CheapShot2 <$> liftRunMessage msg attrs

newtype CheapShot2Effect = CheapShot2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cheapShot2Effect :: EffectArgs -> CheapShot2Effect
cheapShot2Effect = cardEffect CheapShot2Effect Cards.cheapShot2

instance RunMessage CheapShot2Effect where
  runMessage msg e@(CheapShot2Effect attrs) = runQueueT $ case msg of
    EndTurn iid | toTarget iid == attrs.target -> do
      case attrs.meta of
        Just (EffectMetaTarget (CardIdTarget cardId)) -> do
          disable attrs
          returnToHand iid cardId
        _ -> error "invalid meta target"
      pure e
    _ -> CheapShot2Effect <$> liftRunMessage msg attrs
