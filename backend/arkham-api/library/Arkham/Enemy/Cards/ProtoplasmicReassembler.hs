module Arkham.Enemy.Cards.ProtoplasmicReassembler (protoplasmicReassembler) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ProtoplasmicReassembler = ProtoplasmicReassembler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

protoplasmicReassembler :: EnemyCard ProtoplasmicReassembler
protoplasmicReassembler = enemy ProtoplasmicReassembler Cards.protoplasmicReassembler (2, Static 6, 2) (1, 2)

instance RunMessage ProtoplasmicReassembler where
  runMessage msg (ProtoplasmicReassembler attrs) = runQueueT $ case msg of
    _ -> ProtoplasmicReassembler <$> liftRunMessage msg attrs
