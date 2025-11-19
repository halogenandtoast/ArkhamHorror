module Arkham.Enemy.Cards.SkitteringNonsense (skitteringNonsense) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Shuffle (getCanShuffleIn)
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
      tekelili <- getTekelili 1
      getCanShuffleIn iid tekelili >>= \case
        True -> addTekelili iid tekelili
        False -> initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> SkitteringNonsense <$> liftRunMessage msg attrs
