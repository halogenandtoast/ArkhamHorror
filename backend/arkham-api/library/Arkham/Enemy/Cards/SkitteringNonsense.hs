module Arkham.Enemy.Cards.SkitteringNonsense (skitteringNonsense, SkitteringNonsense (..)) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Capability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Matcher

newtype SkitteringNonsense = SkitteringNonsense EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

skitteringNonsense :: EnemyCard SkitteringNonsense
skitteringNonsense = enemy SkitteringNonsense Cards.skitteringNonsense (2, Static 2, 4) (1, 1)

instance HasAbilities SkitteringNonsense where
  getAbilities (SkitteringNonsense a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDefeated #when You ByAny (be a)

instance RunMessage SkitteringNonsense where
  runMessage msg e@(SkitteringNonsense attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canManipulateDeck <- can.manipulate.deck iid
      tekelili <- getTekelili 1

      if not canManipulateDeck || null tekelili
        then initiateEnemyAttack attrs (attrs.ability 1) iid
        else addTekelili iid tekelili

      pure e
    _ -> SkitteringNonsense <$> liftRunMessage msg attrs
