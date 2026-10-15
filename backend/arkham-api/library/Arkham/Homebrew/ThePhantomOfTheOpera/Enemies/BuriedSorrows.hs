module Arkham.Homebrew.ThePhantomOfTheOpera.Enemies.BuriedSorrows (buriedSorrows) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers (ExposeRoute (..), afterExposedViaFor)
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype BuriedSorrows = BuriedSorrows EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

buriedSorrows :: EnemyCard BuriedSorrows
buriedSorrows = enemy BuriedSorrows Cards.buriedSorrows

instance HasAbilities BuriedSorrows where
  getAbilities (BuriedSorrows a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ afterExposedViaFor (Just You) ExposedByDiscover a

instance RunMessage BuriedSorrows where
  runMessage msg e@(BuriedSorrows attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> BuriedSorrows <$> liftRunMessage msg attrs
