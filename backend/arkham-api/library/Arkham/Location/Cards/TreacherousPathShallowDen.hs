module Arkham.Location.Cards.TreacherousPathShallowDen (treacherousPathShallowDen) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation (createExhausted)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Phase

newtype TreacherousPathShallowDen = TreacherousPathShallowDen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

treacherousPathShallowDen :: LocationCard TreacherousPathShallowDen
treacherousPathShallowDen = location TreacherousPathShallowDen Cards.treacherousPathShallowDen 0 (Static 1)

instance HasAbilities TreacherousPathShallowDen where
  getAbilities (TreacherousPathShallowDen a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)

instance RunMessage TreacherousPathShallowDen where
  runMessage msg l@(TreacherousPathShallowDen attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      coralStarSpawn <- getSetAsideCard Enemies.coralStarSpawn
      enemy <- createEnemyWith coralStarSpawn attrs.id createExhausted
      nextPhaseModifier UpkeepPhase (attrs.ability 1) enemy DoesNotReadyDuringUpkeep
      pure l
    _ -> TreacherousPathShallowDen <$> liftRunMessage msg attrs
