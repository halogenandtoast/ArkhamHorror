module Arkham.Event.Cards.HitMe (hitMe, HitMe (..)) where

import Arkham.ChaosBag.RevealStrategy
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Modifier
import Arkham.RequestedChaosTokenStrategy

newtype HitMe = HitMe EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hitMe :: EventCard HitMe
hitMe = event HitMe Cards.hitMe

instance RunMessage HitMe where
  runMessage msg e@(HitMe attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      push $ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 1) SetAside
      pure e
    RequestedChaosTokens (isSource attrs -> True) miid tokens -> do
      for_ tokens $ \token -> do
        skillTestModifier attrs (ChaosTokenTarget token) NegativeToPositive
      push $ RequestedChaosTokens SkillTestSource miid tokens
      pure e
    _ -> HitMe <$> liftRunMessage msg attrs
