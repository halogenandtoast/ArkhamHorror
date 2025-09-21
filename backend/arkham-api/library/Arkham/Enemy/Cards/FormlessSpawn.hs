module Arkham.Enemy.Cards.FormlessSpawn (formlessSpawn) where

import Arkham.Asset.Types (Field (..))
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Modifiers qualified as Mod
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Treachery.Types (Field (..))

newtype FormlessSpawn = FormlessSpawn EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

formlessSpawn :: EnemyCard FormlessSpawn
formlessSpawn =
  enemy FormlessSpawn Cards.formlessSpawn (2, Static 10, 2) (3, 3)
    & setSpawnAt (locationIs Locations.nexusOfNKai)

instance HasModifiersFor FormlessSpawn where
  getModifiersFor (FormlessSpawn a) = do
    doomCount <-
      getSum
        <$> foldAllM
          [ selectAgg Sum EnemyDoom $ EnemyAt $ locationIs Locations.nexusOfNKai
          , selectAgg Sum TreacheryDoom $ TreacheryAt $ locationIs Locations.nexusOfNKai
          , selectAgg Sum AssetDoom $ AssetAt $ locationIs Locations.nexusOfNKai
          , selectAgg Sum InvestigatorDoom $ InvestigatorAt $ locationIs Locations.nexusOfNKai
          , selectAgg Sum LocationDoom $ locationIs Locations.nexusOfNKai
          ]

    modifySelf a [CannotMove, CannotBeMoved, Mod.EnemyFight doomCount, Mod.EnemyEvade doomCount]

instance RunMessage FormlessSpawn where
  runMessage msg (FormlessSpawn attrs) = FormlessSpawn <$> runMessage msg attrs
