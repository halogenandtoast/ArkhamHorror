module Arkham.Enemy.Cards.AbigailForemanWaryLibrarian (abigailForemanWaryLibrarian) where

import Arkham.Ability
import Arkham.Campaigns.BrethrenOfAsh.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Projection

newtype AbigailForemanWaryLibrarian = AbigailForemanWaryLibrarian EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abigailForemanWaryLibrarian :: EnemyCard AbigailForemanWaryLibrarian
abigailForemanWaryLibrarian =
  enemy AbigailForemanWaryLibrarian Cards.abigailForemanWaryLibrarian (2, Static 4, 4) (0, 1)

instance HasAbilities AbigailForemanWaryLibrarian where
  getAbilities (AbigailForemanWaryLibrarian a) =
    extend1 a
      $ restricted a 1 OnSameLocation
      $ ActionAbility #parley (Just $ AbilitySkill #intellect) (ActionCost 1)

instance RunMessage AbigailForemanWaryLibrarian where
  runMessage msg e@(AbigailForemanWaryLibrarian attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mLoc <- field EnemyLocation attrs.id
      for_ mLoc \loc -> do
        clues <- field LocationClues loc
        sid <- getRandom
        beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed clues)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      mLoc <- field EnemyLocation attrs.id
      for_ mLoc \loc -> placeClues (attrs.ability 1) loc 1
      codex iid (attrs.ability 1) 5
      pure e
    _ -> AbigailForemanWaryLibrarian <$> liftRunMessage msg attrs
