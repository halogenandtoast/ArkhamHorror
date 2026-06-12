module Arkham.Location.Cards.ArkhamPoliceStationByTheBook (arkhamPoliceStationByTheBook) where

import Arkham.Ability
import Arkham.Enemy.Types (Field (..))
import Arkham.GameValue
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Trait (Trait (Cultist))

newtype ArkhamPoliceStationByTheBook = ArkhamPoliceStationByTheBook LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamPoliceStationByTheBook :: LocationCard ArkhamPoliceStationByTheBook
arkhamPoliceStationByTheBook =
  location ArkhamPoliceStationByTheBook Cards.arkhamPoliceStationByTheBook 4 (Static 2)

connectedTitles :: LocationMatcher
connectedTitles = mapOneOf LocationWithTitle ["Rivertown", "Downtown", "Easttown"]

instance HasModifiersFor ArkhamPoliceStationByTheBook where
  getModifiersFor (ArkhamPoliceStationByTheBook a) = do
    modifySelf a [ConnectedToWhen (be a) connectedTitles]
    modifySelect a connectedTitles [ConnectedToWhen Anywhere (be a)]

instance HasAbilities ArkhamPoliceStationByTheBook where
  getAbilities (ArkhamPoliceStationByTheBook a) =
    extendRevealed
      a
      [ restricted a 1 Here $ FastAbility Free
      , restricted
          a
          2
          ( Here
              <> youExist InvestigatorWithAnyClues
              <> exists (enemyAt a <> EnemyWithTrait Cultist <> NonWeaknessEnemy)
          )
          $ FastAbility' Free #parley
      ]

instance RunMessage ArkhamPoliceStationByTheBook where
  runMessage msg l@(ArkhamPoliceStationByTheBook attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- getAccessibleLocations iid (attrs.ability 1)
      chooseTargetM iid locations (moveTo (attrs.ability 1) iid)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      clues <- field InvestigatorClues iid
      enemies <- select $ enemyAt attrs <> EnemyWithTrait Cultist <> NonWeaknessEnemy
      choices <- forMaybeM enemies \enemy -> do
        remaining <- field EnemyRemainingHealth enemy
        pure $ remaining >>= \n -> guard (n <= clues) $> (enemy, n)
      chooseOneM iid do
        for_ choices \(enemy, n) -> targeting enemy do
          spendClues iid n
          addToVictory iid enemy
      pure l
    _ -> ArkhamPoliceStationByTheBook <$> liftRunMessage msg attrs
