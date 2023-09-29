module Arkham.Enemy.Cards.FormlessSpawn (
  formlessSpawn,
  FormlessSpawn (..),
) where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner hiding (enemyDoom)
import Arkham.Helpers.Modifiers qualified as Mod
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Treachery.Types (Field (..))

newtype FormlessSpawn = FormlessSpawn EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

formlessSpawn :: EnemyCard FormlessSpawn
formlessSpawn =
  enemyWith
    FormlessSpawn
    Cards.formlessSpawn
    (2, Static 10, 2)
    (3, 3)
    (spawnAtL ?~ SpawnAt (locationIs Locations.nexusOfNKai))

instance HasModifiersFor FormlessSpawn where
  getModifiersFor target (FormlessSpawn a) | isTarget a target = do
    enemyDoom <-
      selectAgg Sum EnemyDoom
        $ EnemyAt
        $ locationIs
          Locations.nexusOfNKai
    treacheryDoom <-
      selectAgg Sum TreacheryDoom
        $ TreacheryAt
        $ locationIs
          Locations.nexusOfNKai
    assetDoom <-
      selectAgg Sum AssetDoom
        $ AssetAt
        $ locationIs
          Locations.nexusOfNKai
    investigatorDoom <-
      selectAgg Sum InvestigatorDoom
        $ InvestigatorAt
        $ locationIs
          Locations.nexusOfNKai
    nexusDoom <- selectAgg Sum LocationDoom $ locationIs Locations.nexusOfNKai

    let
      doomCount =
        getSum
          $ fold
            [enemyDoom, treacheryDoom, assetDoom, investigatorDoom, nexusDoom]

    pure
      $ toModifiers
        a
        [ CannotMove
        , CannotBeMoved
        , Mod.EnemyFight doomCount
        , Mod.EnemyEvade doomCount
        ]
  getModifiersFor _ _ = pure []

instance RunMessage FormlessSpawn where
  runMessage msg (FormlessSpawn attrs) = FormlessSpawn <$> runMessage msg attrs
