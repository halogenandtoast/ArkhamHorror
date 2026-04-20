module Arkham.Enemy.Cards.DarkMagician (darkMagician) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype DarkMagician = DarkMagician EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkMagician :: EnemyCard DarkMagician
darkMagician = enemy DarkMagician Cards.darkMagician (4, Static 4, 2) (1, 1)

instance HasAbilities DarkMagician where
  getAbilities (DarkMagician a) =
    extend1 a $ restricted a 1 OnSameLocation $ forced $ RoundEnds #when

instance RunMessage DarkMagician where
  runMessage msg e@(DarkMagician attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- Find enemy with no doom nearest to Dark Magician
      mLoc <- field EnemyLocation (toId attrs)
      case mLoc of
        Nothing -> pure ()
        Just loc -> do
          nearestEnemies <- select 
            $ NearestEnemyToLocation loc 
            $ EnemyWithDoom (EqualTo $ Static 0) 
            <> NotEnemy (be attrs)
          case nearestEnemies of
            [] -> pure ()
            (targetEnemy : _) -> placeDoom (attrs.ability 1) targetEnemy 1
      pure e
    _ -> DarkMagician <$> liftRunMessage msg attrs
