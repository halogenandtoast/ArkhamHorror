module Arkham.Location.Cards.KualaLumpurStationWestWing (kualaLumpurStationWestWing) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Scenarios.ShadesOfSuffering.Helpers

newtype KualaLumpurStationWestWing = KualaLumpurStationWestWing LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kualaLumpurStationWestWing :: LocationCard KualaLumpurStationWestWing
kualaLumpurStationWestWing =
  symbolLabel $ location KualaLumpurStationWestWing Cards.kualaLumpurStationWestWing 4 (PerPlayer 1)

instance HasAbilities KualaLumpurStationWestWing where
  getAbilities (KualaLumpurStationWestWing a) =
    extendRevealed
      a
      [ scenarioI18n $ withI18nTooltip "kualaLumpurStationWestWing.resign" $ resignAction a
      , restricted
          a
          1
          ( Here
              <> oneOf
                [not_ $ Remembered FoundACheapMemento, youExist $ InvestigatorWithHealableHorror (a.ability 1)]
          )
          $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) (be a))
      ]

instance RunMessage KualaLumpurStationWestWing where
  runMessage msg l@(KualaLumpurStationWestWing attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      healHorror iid (attrs.ability 1) 2
      remember FoundACheapMemento
      pure l
    _ -> KualaLumpurStationWestWing <$> liftRunMessage msg attrs
