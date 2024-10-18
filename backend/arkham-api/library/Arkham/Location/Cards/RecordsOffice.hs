module Arkham.Location.Cards.RecordsOffice (recordsOffice, RecordsOffice (..)) where

import Arkham.Ability
import Arkham.Game.Helpers (getActions)
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (defaultWindows)

newtype RecordsOffice = RecordsOffice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

recordsOffice :: LocationCard RecordsOffice
recordsOffice = location RecordsOffice Cards.recordsOffice 3 (PerPlayer 2)

instance HasAbilities RecordsOffice where
  getAbilities (RecordsOffice attrs) =
    extendRevealed1 attrs
      $ restricted attrs 1 (Here <> youExist (not_ (InvestigatorEngagedWith AnyEnemy)))
      $ ActionAbility [] (ActionCost 3)

instance RunMessage RecordsOffice where
  runMessage msg l@(RecordsOffice attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      doStep 1 msg
      pure l
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) -> do
      available <- withGrantedAction iid attrs do
        filterM (<=~> (BasicAbility <> #investigate)) =<< getActions iid (defaultWindows iid)
      chooseOneM iid $ for_ available $ \ab ->
        abilityLabeled iid (decreaseAbilityActionCost ab 1) do
          when (n < 4) $ doStep (n + 1) msg'
      pure l
    _ -> RecordsOffice <$> liftRunMessage msg attrs
