module Arkham.Asset.Assets.DreamsOfDestructionTheFutureIsNotFixed (dreamsOfDestructionCompleted) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)

newtype DreamsOfDestructionTheFutureIsNotFixed = DreamsOfDestructionTheFutureIsNotFixed AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamsOfDestructionCompleted :: AssetCard DreamsOfDestructionTheFutureIsNotFixed
dreamsOfDestructionCompleted =
  asset DreamsOfDestructionTheFutureIsNotFixed Cards.dreamsOfDestructionCompleted

instance HasModifiersFor DreamsOfDestructionTheFutureIsNotFixed where
  getModifiersFor (DreamsOfDestructionTheFutureIsNotFixed a) =
    for_ a.controller \iid -> modified_ a iid [AdditionalSlot #arcane, SanityModifier 2]

instance RunMessage DreamsOfDestructionTheFutureIsNotFixed where
  runMessage msg (DreamsOfDestructionTheFutureIsNotFixed attrs) =
    runQueueT $ DreamsOfDestructionTheFutureIsNotFixed <$> liftRunMessage msg attrs
