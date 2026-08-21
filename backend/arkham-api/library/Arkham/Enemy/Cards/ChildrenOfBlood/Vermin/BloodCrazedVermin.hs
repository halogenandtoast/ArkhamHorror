module Arkham.Enemy.Cards.ChildrenOfBlood.Vermin.BloodCrazedVermin (bloodCrazedVermin) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.Vermin qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype BloodCrazedVermin = BloodCrazedVermin EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

bloodCrazedVermin :: EnemyCard BloodCrazedVermin
bloodCrazedVermin =
  enemy BloodCrazedVermin Cards.bloodCrazedVermin
    & setPrey (InvestigatorWithMostSealedChaosToken #blood)
