module Arkham.Location.Cards.ArkhamPoliceStationByTheBook (arkhamPoliceStationByTheBook) where

import Arkham.Ability
import Arkham.Enemy.Types (Field (..))
import Arkham.GameValue
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.ByTheBook.Helpers (scenarioI18n)

newtype ArkhamPoliceStationByTheBook = ArkhamPoliceStationByTheBook LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamPoliceStationByTheBook :: LocationCard ArkhamPoliceStationByTheBook
arkhamPoliceStationByTheBook =
  location ArkhamPoliceStationByTheBook Cards.arkhamPoliceStationByTheBook 2 (Static 4)

connectedTitles :: LocationMatcher
connectedTitles = mapOneOf LocationWithTitle ["Rivertown", "Downtown", "Easttown"]

instance HasModifiersFor ArkhamPoliceStationByTheBook where
  getModifiersFor (ArkhamPoliceStationByTheBook a) = do
    modifySelf a [ConnectedToWhen (be a) connectedTitles]
    modifySelect a connectedTitles [ConnectedToWhen Anywhere (be a)]

instance HasAbilities ArkhamPoliceStationByTheBook where
  getAbilities (ArkhamPoliceStationByTheBook a) =
    scenarioI18n
      $ extendRevealed
        a
        [ withI18nTooltip "arkhamPoliceStation.moveToConnecting"
            $ restricted a 1 Here
            $ FastAbility Free
        , withI18nTooltip "arkhamPoliceStation.captureCultist"
            $ restricted a 2 Here
            $ FastAbility'
              ( ChooseEnemyCostAndMaybeFieldClueCost
                  (at_ (be a) <> NonWeaknessEnemy <> #cultist)
                  EnemyRemainingHealth
              )
              #parley
        ]

instance RunMessage ArkhamPoliceStationByTheBook where
  runMessage msg l@(ArkhamPoliceStationByTheBook attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- getAccessibleLocations iid (attrs.ability 1)
      chooseTargetM iid locations (moveTo (attrs.ability 1) iid)
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ (chosenEnemyPayment -> menemy) -> do
      -- The enemy choice and the clue spend (equal to its remaining health) are
      -- both handled by the ChooseEnemyCostAndMaybeFieldClueCost cost; here we
      -- only resolve the effect on the chosen enemy.
      for_ menemy (addToVictory iid)
      pure l
    _ -> ArkhamPoliceStationByTheBook <$> liftRunMessage msg attrs
