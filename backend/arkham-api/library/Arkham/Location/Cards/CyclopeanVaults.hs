module Arkham.Location.Cards.CyclopeanVaults (cyclopeanVaults) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheCityOfArchives.Helpers

newtype CyclopeanVaults = CyclopeanVaults LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cyclopeanVaults :: LocationCard CyclopeanVaults
cyclopeanVaults = location CyclopeanVaults Cards.cyclopeanVaults 5 (PerPlayer 1)

instance HasAbilities CyclopeanVaults where
  getAbilities (CyclopeanVaults a) =
    scenarioI18n
      $ extendRevealed
        a
        [ withI18nTooltip "cyclopeanVaults.discard"
            $ restricted
              a
              1
              (Here <> youExist (HandWith $ HasCard (DiscardableCard <> NonWeakness)))
              actionAbility
        , withI18nTooltip "cyclopeanVaults.remember"
            $ restricted
              a
              2
              (Here <> youExist (HandWith NoCards) <> not_ (Remembered ReadAboutEarth))
              actionAbility
        , restricted
            a
            3
            (Here <> youExist (HandWith $ LengthIs $ lessThan 5))
            doubleActionAbility
        ]

instance RunMessage CyclopeanVaults where
  runMessage msg l@(CyclopeanVaults attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardAll iid (attrs.ability 1) NonWeakness
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      remember ReadAboutEarth
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      n <- fieldMap InvestigatorHand length iid
      drawCards iid (attrs.ability 3) (5 - n)
      pure l
    _ -> CyclopeanVaults <$> liftRunMessage msg attrs
