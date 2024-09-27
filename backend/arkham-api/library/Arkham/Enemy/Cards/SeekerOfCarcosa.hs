module Arkham.Enemy.Cards.SeekerOfCarcosa (seekerOfCarcosa, SeekerOfCarcosa (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype SeekerOfCarcosa = SeekerOfCarcosa EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seekerOfCarcosa :: EnemyCard SeekerOfCarcosa
seekerOfCarcosa =
  enemyWith SeekerOfCarcosa Cards.seekerOfCarcosa (2, Static 3, 2) (0, 1)
    $ spawnAtL
    ?~ SpawnAt (EmptyLocation <> "Historical Society")

instance HasAbilities SeekerOfCarcosa where
  getAbilities (SeekerOfCarcosa attrs) =
    extend1 attrs $ mkAbility attrs 1 $ forced $ PhaseEnds #when #mythos

instance RunMessage SeekerOfCarcosa where
  runMessage msg e@(SeekerOfCarcosa attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      enemyLocation <- field EnemyLocation attrs.id
      for_ enemyLocation \loc -> do
        clueCount <- field LocationClues loc
        if clueCount > 0
          then do
            removeTokens source (toTarget loc) #clue 1
            placeClues source (toTarget attrs) 1
          else placeDoom source attrs 1
      pure e
    _ -> SeekerOfCarcosa <$> liftRunMessage msg attrs
