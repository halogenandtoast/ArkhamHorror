module Arkham.Enemy.Cards.CorpseTaker (
  CorpseTaker (..),
  corpseTaker,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Token

newtype CorpseTaker = CorpseTaker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corpseTaker :: EnemyCard CorpseTaker
corpseTaker =
  enemyWith CorpseTaker Cards.corpseTaker (4, Static 3, 3) (1, 2)
    $ spawnAtL ?~ SpawnLocation (FarthestLocationFromYou EmptyLocation)

instance HasAbilities CorpseTaker where
  getAbilities (CorpseTaker x) =
    withBaseAbilities x
      $ [ mkAbility x 1 $ ForcedAbility $ PhaseEnds #when #mythos
        , mkAbility x 2 $ ForcedAbility $ PhaseEnds #when #enemy
        ]

instance RunMessage CorpseTaker where
  runMessage msg e@(CorpseTaker attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ placeDoom (toAbilitySource attrs 1) attrs 1
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      enemyLocation <- field EnemyLocation (toId attrs)
      case enemyLocation of
        Nothing -> pure e
        Just loc -> do
          mRivertown <- selectOne $ LocationWithTitle "Rivertown"
          mMainPath <- selectOne $ LocationWithTitle "Main Path"
          let location = fromJustNote "one of these has to exist" (mRivertown <|> mMainPath)
          if loc == location
            then do
              pushAll $ replicate (enemyDoom attrs) PlaceDoomOnAgenda
              pure $ CorpseTaker $ attrs & tokensL %~ removeAllTokens Doom
            else do
              lead <- getLead
              locations <- selectList $ ClosestPathLocation loc location
              pushWhen (notNull locations)
                $ chooseOrRunOne lead
                $ targetLabels locations (only . EnemyMove (toId attrs))
              pure e
    _ -> CorpseTaker <$> runMessage msg attrs
