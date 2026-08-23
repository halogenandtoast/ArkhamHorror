module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.ManagersOfficeNight (managersOfficeNight) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype ManagersOfficeNight = ManagersOfficeNight LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

managersOfficeNight :: LocationCard ManagersOfficeNight
managersOfficeNight = symbolLabel $ location ManagersOfficeNight Cards.managersOfficeNight 4 (PerPlayer 1)

instance HasModifiersFor ManagersOfficeNight where
  getModifiersFor (ManagersOfficeNight a) = unless a.revealed do
    foundKeys <- remembered TheInvestigatorsFoundTheManagersKeys
    unless foundKeys $ modifySelect a Anyone [CannotEnter a.id]

instance HasAbilities ManagersOfficeNight where
  getAbilities (ManagersOfficeNight a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> NoCluesOnThis)
      $ FastAbility (AddTokenCost 1 #blood)

instance RunMessage ManagersOfficeNight where
  runMessage msg l@(ManagersOfficeNight attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      remember TheInvestigatorsFoundASheetOfArcaneSymbols
      pure l
    _ -> ManagersOfficeNight <$> liftRunMessage msg attrs
