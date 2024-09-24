module Arkham.Enemy.Cards.BrianBurnhamWantsOut (brianBurnhamWantsOut, BrianBurnhamWantsOut (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype BrianBurnhamWantsOut = BrianBurnhamWantsOut EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brianBurnhamWantsOut :: EnemyCard BrianBurnhamWantsOut
brianBurnhamWantsOut = enemy BrianBurnhamWantsOut Cards.brianBurnhamWantsOut (3, Static 3, 5) (1, 0)

instance HasAbilities BrianBurnhamWantsOut where
  getAbilities (BrianBurnhamWantsOut a) =
    extend
      a
      [restrictedAbility a 1 OnSameLocation $ parleyAction $ SkillIconCost 3 mempty]

instance RunMessage BrianBurnhamWantsOut where
  runMessage msg e@(BrianBurnhamWantsOut attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeClues attrs attrs =<< perPlayer 1
      place attrs =<< selectJust (LocationWithTitle "First National Grocery")
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moveTokens (attrs.ability 1) attrs iid #clue 1
      doStep 2 msg
      pure e
    DoStep 2 (UseThisAbility _iid (isSource attrs -> True) 1) -> do
      when (attrs.token #clue == 0) $ addToVictory attrs
      pure e
    _ -> BrianBurnhamWantsOut <$> liftRunMessage msg attrs
