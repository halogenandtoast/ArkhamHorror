module Arkham.Types.Enemy.Cards.CorpseTaker
  ( CorpseTaker(..)
  , corpseTaker
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message

newtype CorpseTaker = CorpseTaker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities env)

corpseTaker :: EnemyCard CorpseTaker
corpseTaker = enemyWith
  CorpseTaker
  Cards.corpseTaker
  (4, Static 3, 3)
  (1, 2)
  (spawnAtL ?~ FarthestLocationFromYou EmptyLocation)

instance EnemyRunner env => RunMessage env CorpseTaker where
  runMessage msg e@(CorpseTaker attrs@EnemyAttrs {..}) = case msg of
    EndMythos -> pure $ CorpseTaker $ attrs & doomL +~ 1
    EndEnemy -> do
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
