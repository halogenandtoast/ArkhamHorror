module Arkham.Location.Cards.CasinoFloorBusyNight (casinoFloorBusyNight) where

import Arkham.Ability
import Arkham.Card
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Story
import Arkham.Placement
import Arkham.ScenarioLogKey
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Token
import Arkham.Trait (Trait (Casino))

newtype CasinoFloorBusyNight = CasinoFloorBusyNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

casinoFloorBusyNight :: LocationCard CasinoFloorBusyNight
casinoFloorBusyNight = symbolLabel $ location CasinoFloorBusyNight Cards.casinoFloorBusyNight 0 (Static 0)

instance HasAbilities CasinoFloorBusyNight where
  getAbilities (CasinoFloorBusyNight a) =
    extendRevealed
      a
      [ scenarioI18n $ withI18nTooltip "casinoFloorBusyNight.resign" $ locationResignAction a
      , restricted
          a
          1
          ( Here
              <> Remembered ImpersonatedAHighRoller
              <> youExist (HasTokens AlarmLevel $ LessThanOrEqualTo $ Static 6)
          )
          $ actionAbilityWithCost (ResourceCost 2)
      ]

instance RunMessage CasinoFloorBusyNight where
  runMessage msg l@(CasinoFloorBusyNight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      findEncounterCard iid attrs $ card_ $ #enemy <> CardWithTrait Casino
      pure l
    FoundEncounterCard _iid (isTarget attrs -> True) (toCard -> card) -> do
      enemy <- createEnemyAt card attrs
      packageDelivery <- fetchCard Stories.packageDelivery
      push $ PlaceStory packageDelivery (AttachedToEnemy enemy)
      pure l
    _ -> CasinoFloorBusyNight <$> liftRunMessage msg attrs
