module Arkham.Enemy.Cards.NathanWickMasterOfIndoctrination (nathanWickMasterOfIndoctrination) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype NathanWickMasterOfIndoctrination = NathanWickMasterOfIndoctrination EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nathanWickMasterOfIndoctrination :: EnemyCard NathanWickMasterOfIndoctrination
nathanWickMasterOfIndoctrination =
  enemy
    NathanWickMasterOfIndoctrination
    Cards.nathanWickMasterOfIndoctrination
    (4, Static 5, 3)
    (1, 1)

instance HasAbilities NathanWickMasterOfIndoctrination where
  getAbilities (NathanWickMasterOfIndoctrination a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ SkillTestResult #after You (WhileEvadingAnEnemy $ be a) (SuccessResult $ atLeast 3)

instance RunMessage NathanWickMasterOfIndoctrination where
  runMessage msg e@(NathanWickMasterOfIndoctrination attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.keys (placeKey iid)
      addToVictory attrs
      pure e
    _ -> NathanWickMasterOfIndoctrination <$> liftRunMessage msg attrs
