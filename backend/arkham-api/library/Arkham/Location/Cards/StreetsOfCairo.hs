module Arkham.Location.Cards.StreetsOfCairo (streetsOfCairo) where

import Arkham.Ability
import Arkham.Helpers.History
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Window (windowType)
import Arkham.Window qualified as Window

newtype StreetsOfCairo = StreetsOfCairo LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

streetsOfCairo :: LocationCard StreetsOfCairo
streetsOfCairo = symbolLabel $ location StreetsOfCairo Cards.streetsOfCairo 3 (PerPlayer 2)

instance HasAbilities StreetsOfCairo where
  getAbilities (StreetsOfCairo a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ DiscoverClues #after You (be a) (atLeast 1)
      , skillTestAbility $ restricted a 2 (Here <> thisExists a LocationWithoutClues) actionAbility
      ]

instance RunMessage StreetsOfCairo where
  runMessage msg l@(StreetsOfCairo attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      let n = sum [x | (windowType -> Window.DiscoverClues _ lid _ x) <- ws, lid == attrs.id]
      total <- findWithDefault 0 attrs.id <$> getHistoryField TurnHistory iid HistoryCluesDiscovered
      when (total >= 2 && total - n < 2) $ assignHorror iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) attrs #agility (Fixed 4)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      remember FoundADoorMarkedWithBlood
      pure l
    _ -> StreetsOfCairo <$> liftRunMessage msg attrs
