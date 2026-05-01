module Arkham.Enemy.Cards.AbigailForemanWaryLibrarian (abigailForemanWaryLibrarian) where

import Arkham.Ability
import Arkham.Campaigns.BrethrenOfAsh.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Location.Types (Field (..))

newtype AbigailForemanWaryLibrarian = AbigailForemanWaryLibrarian EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abigailForemanWaryLibrarian :: EnemyCard AbigailForemanWaryLibrarian
abigailForemanWaryLibrarian =
  enemy AbigailForemanWaryLibrarian Cards.abigailForemanWaryLibrarian (2, Static 4, 4) (0, 1)

instance HasAbilities AbigailForemanWaryLibrarian where
  getAbilities (AbigailForemanWaryLibrarian a) =
    extend1 a $ restricted a 1 OnSameLocation $ ActionAbility #parley #intellect (ActionCost 1)

instance RunMessage AbigailForemanWaryLibrarian where
  runMessage msg e@(AbigailForemanWaryLibrarian attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect
        $ EnemyLocationFieldCalculation attrs.id LocationClues
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      withLocationOf attrs.id $ placeCluesOn (attrs.ability 1) 1
      codex iid (attrs.ability 1) 5
      pure e
    _ -> AbigailForemanWaryLibrarian <$> liftRunMessage msg attrs
