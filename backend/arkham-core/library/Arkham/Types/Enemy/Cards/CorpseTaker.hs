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
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.LocationMatcher
import Arkham.Types.Message

newtype CorpseTaker = CorpseTaker EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corpseTaker :: EnemyCard CorpseTaker
corpseTaker = enemy CorpseTaker Cards.corpseTaker
  $ (healthDamageL .~ 1)
  . (sanityDamageL .~ 2)
  . (fightL .~ 4)
  . (healthL .~ Static 3)
  . (evadeL .~ 3)
  . (spawnAtL ?~ FarthestLocationFromYou EmptyLocation)

instance HasModifiersFor env CorpseTaker where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env CorpseTaker where
  getActions i window (CorpseTaker attrs) = getActions i window attrs

instance EnemyRunner env => RunMessage env CorpseTaker where
  runMessage msg e@(CorpseTaker attrs@EnemyAttrs {..}) = case msg of
    EndMythos -> pure $ CorpseTaker $ attrs & doomL +~ 1
    EndEnemy -> do
      mRivertown <- getLocationIdWithTitle "Rivertown"
      mMainPath <- getLocationIdWithTitle "Main Path"
      let
        locationId =
          fromJustNote "one of these has to exist" (mRivertown <|> mMainPath)
      if enemyLocation == locationId
        then do
          unshiftMessages (replicate enemyDoom PlaceDoomOnAgenda)
          pure $ CorpseTaker $ attrs & doomL .~ 0
        else do
          leadInvestigatorId <- getLeadInvestigatorId
          closestLocationIds <- map unClosestPathLocationId
            <$> getSetList (enemyLocation, locationId)
          case closestLocationIds of
            [lid] -> e <$ unshiftMessage (EnemyMove enemyId enemyLocation lid)
            lids -> e <$ unshiftMessage
              (chooseOne
                leadInvestigatorId
                [ EnemyMove enemyId enemyLocation lid | lid <- lids ]
              )
    _ -> CorpseTaker <$> runMessage msg attrs
