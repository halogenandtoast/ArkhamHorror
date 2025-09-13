module Arkham.Enemy.Cards.InnsmouthTroublemaker (innsmouthTroublemaker) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.ForMovement
import Arkham.Matcher

newtype InnsmouthTroublemaker = InnsmouthTroublemaker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innsmouthTroublemaker :: EnemyCard InnsmouthTroublemaker
innsmouthTroublemaker =
  enemyWith InnsmouthTroublemaker Cards.innsmouthTroublemaker (4, Static 3, 2) (2, 0)
    $ spawnAtL
    ?~ SpawnAt (LocationWithMostClues Anywhere)

instance HasAbilities InnsmouthTroublemaker where
  getAbilities (InnsmouthTroublemaker a) =
    extend1 a
      $ restricted a 1 (youExist $ at_ $ orConnected NotForMovement $ locationWithEnemy a.id)
      $ parleyAction
      $ PlaceClueOnLocationCost 1

instance RunMessage InnsmouthTroublemaker where
  runMessage msg e@(InnsmouthTroublemaker attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      exhaustThis attrs
      doesNotReadyDuringUpkeep (attrs.ability 1) attrs
      pure e
    _ -> InnsmouthTroublemaker <$> liftRunMessage msg attrs
