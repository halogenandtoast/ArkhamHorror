module Arkham.Location.Cards.OwnersOffice (ownersOffice) where

import Arkham.Ability
import Arkham.Helpers.Location (getConnectedLocations)
import Arkham.Helpers.Log (remembered)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (remember)
import Arkham.ScenarioLogKey
import Arkham.Scenarios.FortuneAndFolly.Helpers (getAlarmLevel)

newtype OwnersOffice = OwnersOffice LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ownersOffice :: LocationCard OwnersOffice
ownersOffice = symbolLabel $ location OwnersOffice Cards.ownersOffice 0 (PerPlayer 3)

instance HasModifiersFor OwnersOffice where
  getModifiersFor (OwnersOffice a) = do
    stoleKeys <- remembered StoleAbarransKeys
    connected <- getConnectedLocations a.id
    alarmLevels <- traverse getAlarmLevel =<< select (investigatorAt a.id)
    let x = getMax0 $ foldMap Max0 alarmLevels
    modifySelf
      a
      $ ShroudModifier x
      : [ AdditionalCostToEnter
            (OrCost [GroupClueCost (PerPlayer 2) (LocationWithId locId) | locId <- connected])
        | not stoleKeys
        ]

instance HasAbilities OwnersOffice where
  getAbilities (OwnersOffice a) =
    extendRevealed1 a $ restricted a 1 (Here <> thisExists a LocationWithoutClues) actionAbility

instance RunMessage OwnersOffice where
  runMessage msg l@(OwnersOffice attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      remember FoundAbarransSigil
      pure l
    _ -> OwnersOffice <$> liftRunMessage msg attrs
