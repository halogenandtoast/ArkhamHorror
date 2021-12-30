module Arkham.Enemy.Cards.CorpseTaker
  ( CorpseTaker(..)
  , corpseTaker
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Timing qualified as Timing

newtype CorpseTaker = CorpseTaker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corpseTaker :: EnemyCard CorpseTaker
corpseTaker = enemyWith
  CorpseTaker
  Cards.corpseTaker
  (4, Static 3, 3)
  (1, 2)
  (spawnAtL ?~ FarthestLocationFromYou EmptyLocation)

instance HasAbilities CorpseTaker where
  getAbilities (CorpseTaker x) = withBaseAbilities
    x
    [ mkAbility x 1 $ ForcedAbility $ PhaseEnds Timing.When $ PhaseIs
      MythosPhase
    , mkAbility x 2 $ ForcedAbility $ PhaseEnds Timing.When $ PhaseIs EnemyPhase
    ]

instance EnemyRunner env => RunMessage env CorpseTaker where
  runMessage msg e@(CorpseTaker attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ pure (PlaceDoom (toTarget attrs) 1)
    UseCardAbility _ source _ 2 _ | isSource attrs source -> do
      mRivertown <- selectOne (LocationWithTitle "Rivertown")
      mMainPath <- selectOne (LocationWithTitle "Main Path")
      let
        locationId =
          fromJustNote "one of these has to exist" (mRivertown <|> mMainPath)
      if enemyLocation == locationId
        then do
          pushAll (replicate enemyDoom PlaceDoomOnAgenda)
          pure $ CorpseTaker $ attrs & doomL .~ 0
        else do
          leadInvestigatorId <- getLeadInvestigatorId
          closestLocationIds <- map unClosestPathLocationId
            <$> getSetList (enemyLocation, locationId, emptyLocationMap)
          case closestLocationIds of
            [lid] -> e <$ push (EnemyMove enemyId enemyLocation lid)
            lids -> e <$ push
              (chooseOne
                leadInvestigatorId
                [ EnemyMove enemyId enemyLocation lid | lid <- lids ]
              )
    _ -> CorpseTaker <$> runMessage msg attrs
