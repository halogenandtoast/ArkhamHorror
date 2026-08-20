module Arkham.Location.Cards.TheDreamEaters.WhereTheGodsDwell.PlateauOfLeng (
  plateauOfLeng,
  PlateauOfLeng (..),
)
where

import Arkham.Ability
import Arkham.Id
import Arkham.Location.CardDefs.TheDreamEaters.WhereTheGodsDwell qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype PlateauOfLeng = PlateauOfLeng LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

plateauOfLeng :: LocationCard PlateauOfLeng
plateauOfLeng = location PlateauOfLeng Cards.plateauOfLeng 3 (PerPlayer 1)

instance HasAbilities PlateauOfLeng where
  getAbilities (PlateauOfLeng attrs) =
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

instance RunMessage PlateauOfLeng where
  runMessage msg l@(PlateauOfLeng attrs) = runQueueT $ case msg of
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
        CheckWindows windows -> CheckWindows $ map replaceWindows windows
        Do (CheckWindows windows) -> Do (CheckWindows $ map replaceWindows windows)
        other -> other

      pure l
    _ -> PlateauOfLeng <$> liftRunMessage msg attrs
