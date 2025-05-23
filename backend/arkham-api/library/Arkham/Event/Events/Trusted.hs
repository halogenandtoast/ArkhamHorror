module Arkham.Event.Events.Trusted (trusted) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Upgrade

newtype Trusted = Trusted EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trusted :: EventCard Trusted
trusted = event Trusted Cards.trusted

instance HasModifiersFor Trusted where
  getModifiersFor (Trusted a) = for_ a.attachedTo.asset \aid ->
    modified_ a aid [HealthModifier 1, SanityModifier 1]

instance RunMessage Trusted where
  runMessage msg e@(Trusted attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      assets <- getUpgradeTargets iid $ assetControlledBy iid <> AllyAsset
      chooseTargetM iid assets \asset -> place attrs $ AttachedToAsset asset Nothing
      pure e
    _ -> Trusted <$> liftRunMessage msg attrs
