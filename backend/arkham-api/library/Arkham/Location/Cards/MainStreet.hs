module Arkham.Location.Cards.MainStreet (mainStreet) where

import Arkham.Ability
import Arkham.Epic.Types (
  GroupOrdinal (..),
  SharedKey (MainStreetEligible, MainStreetReady),
  groupOrdinalKey,
  sharedKeyText,
 )
import Arkham.Helpers.Log (scenarioCount)
import Arkham.Helpers.Modifiers (
  ModifierType (ForMovementConnectedToWhen),
  modifySelect,
  modifySelf,
 )
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.ScenarioLogKey (ScenarioCountKey (EpicShared))

newtype MainStreet = MainStreet LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mainStreet :: LocationCard MainStreet
mainStreet = locationWith MainStreet Cards.mainStreet 3 (PerPlayer 2) connectsToAdjacent

instance HasAbilities MainStreet where
  getAbilities (MainStreet a) =
    [ groupLimit PerGame
        $ restricted
          a
          1
          (Here <> HasScenarioCount (EpicShared $ sharedKeyText MainStreetEligible) (atLeast 1))
          actionAbility
    ]

instance HasModifiersFor MainStreet where
  getModifiersFor (MainStreet a) = do
    let slimyStreets = locationIs Cards.slimyStreets
    self <- modifySelf a [ForMovementConnectedToWhen (be a) slimyStreets]
    others <- modifySelect a slimyStreets [ForMovementConnectedToWhen slimyStreets (be a)]
    pure $ self <> others

instance RunMessage MainStreet where
  runMessage msg l@(MainStreet attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (enemyEngagedWith iid) disengageFromAll
      ordinal <- scenarioCount (EpicShared groupOrdinalKey)
      push $ ScenarioSpecific "blobMainStreetReady" (toJSON iid)
      push $ RaiseShared (MainStreetReady $ GroupOrdinal ordinal) 1
      pure l
    _ -> MainStreet <$> liftRunMessage msg attrs
