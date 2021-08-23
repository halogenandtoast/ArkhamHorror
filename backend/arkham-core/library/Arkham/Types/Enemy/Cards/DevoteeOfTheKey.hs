module Arkham.Types.Enemy.Cards.DevoteeOfTheKey
  ( devoteeOfTheKey
  , DevoteeOfTheKey(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message

newtype DevoteeOfTheKey = DevoteeOfTheKey EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities env)

devoteeOfTheKey :: EnemyCard DevoteeOfTheKey
devoteeOfTheKey = enemyWith
  DevoteeOfTheKey
  Cards.devoteeOfTheKey
  (3, Static 3, 3)
  (1, 1)
  (spawnAtL ?~ LocationWithTitle "Base of the Hill")

instance
  ( Query LocationMatcher env
  , EnemyAttrsRunMessage env
  )
  => RunMessage env DevoteeOfTheKey where
  runMessage msg e@(DevoteeOfTheKey attrs@EnemyAttrs {..}) = case msg of
    EndEnemy -> do
      leadInvestigatorId <- getLeadInvestigatorId
      sentinelPeak <- fromJustNote "missing location"
        <$> selectOne (LocationWithTitle "Sentinel Peak")
      if enemyLocation == sentinelPeak
        then
          e <$ pushAll
            [Discard (toTarget attrs), PlaceDoomOnAgenda, PlaceDoomOnAgenda]
        else do
          choices <- map unClosestPathLocationId
            <$> getSetList (enemyLocation, sentinelPeak, emptyLocationMap)
          case choices of
            [] -> error "should not happen"
            [x] -> e <$ push (EnemyMove enemyId enemyLocation x)
            xs ->
              e
                <$ push
                     (chooseOne leadInvestigatorId
                     $ map (EnemyMove enemyId enemyLocation) xs
                     )
    _ -> DevoteeOfTheKey <$> runMessage msg attrs
