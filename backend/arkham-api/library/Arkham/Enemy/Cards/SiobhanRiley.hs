module Arkham.Enemy.Cards.SiobhanRiley (siobhanRiley) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype SiobhanRiley = SiobhanRiley EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

siobhanRiley :: EnemyCard SiobhanRiley
siobhanRiley = enemy SiobhanRiley Cards.siobhanRiley

instance HasAbilities SiobhanRiley where
  getAbilities (SiobhanRiley a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyEngaged #after You (be a)
      , restricted a 2 (OnSameLocation <> DuringPhase #investigation)
          $ forced
          $ GainsResources #after You AnySource (atLeast 1)
      ]

instance RunMessage SiobhanRiley where
  runMessage msg e@(SiobhanRiley attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- fieldMap InvestigatorResources (`div` 5) iid
      when (n > 0) $ loseResources iid (attrs.ability 1) n
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      when attrs.exhausted $ readyThis attrs
      enemyEngageInvestigator attrs iid
      attackIfEngaged attrs (Just iid)
      pure e
    _ -> SiobhanRiley <$> liftRunMessage msg attrs
