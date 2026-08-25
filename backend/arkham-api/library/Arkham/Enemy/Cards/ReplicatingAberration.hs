module Arkham.Enemy.Cards.ReplicatingAberration (
  replicatingAberrationA,
  replicatingAberrationB,
  replicatingAberrationC,
  replicatingAberrationD,
  replicatingAberrationE,
  replicatingAberrationF,
  replicatingAberrationG,
  replicatingAberrationH,
  replicatingAberrationI,
) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Enemy.CardDefs.Standalone qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (RemoveFromGameInsteadOfDiscard), modifySelf)

newtype ReplicatingAberration = ReplicatingAberration EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mkAberration :: CardDef -> EnemyCard ReplicatingAberration
mkAberration def = enemy ReplicatingAberration def

replicatingAberrationA
  , replicatingAberrationB
  , replicatingAberrationC
    :: EnemyCard ReplicatingAberration
replicatingAberrationD
  , replicatingAberrationE
  , replicatingAberrationF
    :: EnemyCard ReplicatingAberration
replicatingAberrationG
  , replicatingAberrationH
  , replicatingAberrationI
    :: EnemyCard ReplicatingAberration
replicatingAberrationA = mkAberration Cards.replicatingAberrationA
replicatingAberrationB = mkAberration Cards.replicatingAberrationB
replicatingAberrationC = mkAberration Cards.replicatingAberrationC
replicatingAberrationD = mkAberration Cards.replicatingAberrationD
replicatingAberrationE = mkAberration Cards.replicatingAberrationE
replicatingAberrationF = mkAberration Cards.replicatingAberrationF
replicatingAberrationG = mkAberration Cards.replicatingAberrationG
replicatingAberrationH = mkAberration Cards.replicatingAberrationH
replicatingAberrationI = mkAberration Cards.replicatingAberrationI

instance HasModifiersFor ReplicatingAberration where
  getModifiersFor (ReplicatingAberration a) = modifySelf a [RemoveFromGameInsteadOfDiscard]

instance RunMessage ReplicatingAberration where
  runMessage msg (ReplicatingAberration attrs) = runQueueT $ ReplicatingAberration <$> liftRunMessage msg attrs
