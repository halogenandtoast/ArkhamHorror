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
import Arkham.Phase
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Token

newtype CorpseTaker = CorpseTaker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corpseTaker :: EnemyCard CorpseTaker
corpseTaker =
  enemyWith
    CorpseTaker
    Cards.corpseTaker
    (4, Static 3, 3)
    (1, 2)
    (spawnAtL ?~ SpawnLocation (FarthestLocationFromYou EmptyLocation))

instance HasAbilities CorpseTaker where
  getAbilities (CorpseTaker x) =
    withBaseAbilities
      x
      [ mkAbility x 1 $
          ForcedAbility $
            PhaseEnds Timing.When $
              PhaseIs
                MythosPhase
      , mkAbility x 2 $ ForcedAbility $ PhaseEnds Timing.When $ PhaseIs EnemyPhase
      ]

instance RunMessage CorpseTaker where
  runMessage msg e@(CorpseTaker attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      e <$ pure (PlaceTokens (toAbilitySource attrs 1) (toTarget attrs) Doom 1)
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      enemyLocation <- field EnemyLocation enemyId
      case enemyLocation of
        Nothing -> pure e
        Just loc -> do
          mRivertown <- selectOne (LocationWithTitle "Rivertown")
          mMainPath <- selectOne (LocationWithTitle "Main Path")
          let
            locationId =
              fromJustNote
                "one of these has to exist"
                (mRivertown <|> mMainPath)
          if loc == locationId
            then do
              pushAll $ replicate (enemyDoom attrs) PlaceDoomOnAgenda
              pure $ CorpseTaker $ attrs & tokensL %~ removeAllTokens Doom
            else do
              lead <- getLead
              closestLocationIds <- selectList $ ClosestPathLocation loc locationId
              case closestLocationIds of
                [lid] -> push $ EnemyMove enemyId lid
                lids ->
                  push $
                    chooseOne
                      lead
                      [targetLabel lid [EnemyMove enemyId lid] | lid <- lids]
              pure e
    _ -> CorpseTaker <$> runMessage msg attrs
