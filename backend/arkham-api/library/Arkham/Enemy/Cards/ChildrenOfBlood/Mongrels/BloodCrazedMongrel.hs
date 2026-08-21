module Arkham.Enemy.Cards.ChildrenOfBlood.Mongrels.BloodCrazedMongrel (bloodCrazedMongrel) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.Mongrels qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype BloodCrazedMongrel = BloodCrazedMongrel EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

bloodCrazedMongrel :: EnemyCard BloodCrazedMongrel
bloodCrazedMongrel =
  enemy BloodCrazedMongrel Cards.bloodCrazedMongrel
    & setPrey (InvestigatorWithMostSealedChaosToken #blood)
