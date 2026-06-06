module Arkham.Treachery.Cards.RealityAcid (realityAcid) where

import Arkham.ChaosBag.RevealStrategy
import Arkham.RequestedChaosTokenStrategy
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RealityAcid = RealityAcid TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

realityAcid :: TreacheryCard RealityAcid
realityAcid = treachery RealityAcid Cards.realityAcid

instance RunMessage RealityAcid where
  runMessage msg t@(RealityAcid attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- Subject 8L-08 devours a random aspect of reality: reveal two random
      -- chaos tokens from the chaos bag and consult a chart.
      push $ RequestChaosTokens (attrs.ability 1) (Just iid) (Reveal 2) SetAside
      pure t
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just _iid) tokens -> do
      let _faces = map (.face) tokens
      -- TODO: consult chart on revealed tokens (provided separately) to
      -- determine what Subject 8L-08 devoured. The two token faces are in
      -- `_faces`. No devour/effect is performed yet.
      resetChaosTokens (attrs.ability 1)
      pure t
    _ -> RealityAcid <$> liftRunMessage msg attrs
