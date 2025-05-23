module Arkham.Enemy.Cards.CorpseTaker (corpseTaker) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype CorpseTaker = CorpseTaker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corpseTaker :: EnemyCard CorpseTaker
corpseTaker =
  enemy CorpseTaker Cards.corpseTaker (4, Static 3, 3) (1, 2)
    & setSpawnAt (FarthestLocationFromYou EmptyLocation)

instance HasAbilities CorpseTaker where
  getAbilities (CorpseTaker x) =
    extend
      x
      [ mkAbility x 1 $ forced $ PhaseEnds #when #mythos
      , mkAbility x 2 $ forced $ PhaseEnds #when #enemy
      ]

instance RunMessage CorpseTaker where
  runMessage msg e@(CorpseTaker attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      withLocationOf attrs \loc -> do
        mRivertown <- selectOne $ LocationWithTitle "Rivertown"
        mMainPath <- selectOne $ LocationWithTitle "Main Path"
        let location = fromJustNote "one of these has to exist" (mRivertown <|> mMainPath)
        if loc == location
          then do
            removeAllDoom (attrs.ability 2) attrs
            placeDoomOnAgenda attrs.doom
          else do
            lead <- getLead
            locations <- select $ ClosestPathLocation loc location
            chooseOrRunOneM lead $ targets locations (enemyMoveTo attrs)
      pure e
    _ -> CorpseTaker <$> liftRunMessage msg attrs
