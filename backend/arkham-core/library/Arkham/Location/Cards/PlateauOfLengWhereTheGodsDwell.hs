module Arkham.Location.Cards.PlateauOfLengWhereTheGodsDwell (
  plateauOfLengWhereTheGodsDwell,
  PlateauOfLengWhereTheGodsDwell (..),
)
where

import Arkham.Ability
import Arkham.Id
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype PlateauOfLengWhereTheGodsDwell = PlateauOfLengWhereTheGodsDwell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

plateauOfLengWhereTheGodsDwell :: LocationCard PlateauOfLengWhereTheGodsDwell
plateauOfLengWhereTheGodsDwell = location PlateauOfLengWhereTheGodsDwell Cards.plateauOfLengWhereTheGodsDwell 3 (PerPlayer 1)

instance HasAbilities PlateauOfLengWhereTheGodsDwell where
  getAbilities (PlateauOfLengWhereTheGodsDwell attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 1 (notExists EmptyLocation)
          $ forced
          $ EnemyAttemptsToSpawnAt #when AnyEnemy EmptyLocation
      ]

getEnemy :: [Window] -> EnemyId
getEnemy [] = error "Expected a window"
getEnemy ((windowType -> Window.EnemyAttemptsToSpawnAt eid _) : _) = eid
getEnemy (_ : rest) = getEnemy rest

instance RunMessage PlateauOfLengWhereTheGodsDwell where
  runMessage msg l@(PlateauOfLengWhereTheGodsDwell attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getEnemy -> enemy) _ -> do
      let
        replaceWindows = \case
          Window timing (Window.EnemyAttemptsToSpawnAt eid EmptyLocation) batch
            | eid == enemy ->
                Window timing (Window.EnemyAttemptsToSpawnAt eid (LocationWithId attrs.id)) batch
          other -> other

      mapQueue $ \case
        When (EnemySpawnAtLocationMatching miid EmptyLocation eid)
          | eid == enemy ->
              When (EnemySpawnAtLocationMatching miid (LocationWithId attrs.id) eid)
        EnemySpawnAtLocationMatching miid EmptyLocation eid
          | eid == enemy ->
              EnemySpawnAtLocationMatching miid (LocationWithId attrs.id) eid
        After (EnemySpawnAtLocationMatching miid EmptyLocation eid)
          | eid == enemy ->
              After (EnemySpawnAtLocationMatching miid (LocationWithId attrs.id) eid)
        RunWindow iid windows -> RunWindow iid $ map replaceWindows windows
        CheckWindow iids windows -> CheckWindow iids $ map replaceWindows windows
        other -> other

      pure l
    _ -> PlateauOfLengWhereTheGodsDwell <$> liftRunMessage msg attrs
