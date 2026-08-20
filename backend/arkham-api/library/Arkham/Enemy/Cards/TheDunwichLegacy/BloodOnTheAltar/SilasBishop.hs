module Arkham.Enemy.Cards.TheDunwichLegacy.BloodOnTheAltar.SilasBishop (silasBishop) where

import Arkham.Enemy.CardDefs.TheDunwichLegacy.BloodOnTheAltar qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers

newtype SilasBishop = SilasBishop EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

silasBishop :: EnemyCard SilasBishop
silasBishop = enemy SilasBishop Cards.silasBishop

instance HasModifiersFor SilasBishop where
  getModifiersFor (SilasBishop attrs) = modifySelf attrs [CannotMakeAttacksOfOpportunity]

instance RunMessage SilasBishop where
  runMessage msg (SilasBishop attrs) = SilasBishop <$> runMessage msg attrs
