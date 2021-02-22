module Arkham.Types.Enemy.Cards.DevoteeOfTheKey
  ( devoteeOfTheKey
  , DevoteeOfTheKey(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.EnemyId
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.Message

newtype DevoteeOfTheKey = DevoteeOfTheKey EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devoteeOfTheKey :: EnemyId -> DevoteeOfTheKey
devoteeOfTheKey uuid =
  DevoteeOfTheKey
    $ baseAttrs uuid "02294"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 1)
    . (fightL .~ 3)
    . (healthL .~ Static 3)
    . (evadeL .~ 3)
    . (spawnAtL ?~ LocationWithTitle "Base of the Hill")

instance HasModifiersFor env DevoteeOfTheKey where
  getModifiersFor = noModifiersFor

instance EnemyAttrsHasActions env => HasActions env DevoteeOfTheKey where
  getActions i window (DevoteeOfTheKey attrs) = getActions i window attrs

instance EnemyAttrsRunMessage env => RunMessage env DevoteeOfTheKey where
  runMessage msg e@(DevoteeOfTheKey attrs@EnemyAttrs {..}) = case msg of
    EndEnemy -> do
      leadInvestigatorId <- getLeadInvestigatorId
      sentinelPeak <- fromJustNote "missing location"
        <$> getLocationIdWithTitle "Sentinel Peak"
      if enemyLocation == sentinelPeak
        then e <$ unshiftMessages
          [Discard (toTarget attrs), PlaceDoomOnAgenda, PlaceDoomOnAgenda]
        else do
          choices <- map unClosestPathLocationId
            <$> getSetList (enemyLocation, sentinelPeak)
          case choices of
            [] -> error "should not happen"
            [x] -> e <$ unshiftMessage (EnemyMove enemyId enemyLocation x)
            xs -> e <$ unshiftMessage
              (chooseOne leadInvestigatorId
              $ map (EnemyMove enemyId enemyLocation) xs
              )
    _ -> DevoteeOfTheKey <$> runMessage msg attrs
