module Arkham.Location.Cards.AdministrationBuilding (administrationBuilding) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (administrationBuilding)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype AdministrationBuilding = AdministrationBuilding LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

administrationBuilding :: LocationCard AdministrationBuilding
administrationBuilding = symbolLabel $ location AdministrationBuilding Cards.administrationBuilding 4 (PerPlayer 1)

instance HasAbilities AdministrationBuilding where
  getAbilities (AdministrationBuilding x) =
    extendRevealed
      x
      [ restricted x 1 Here $ forced $ RevealLocation #after You (be x)
      , restricted x 2 Here $ forced $ TurnEnds #when You
      ]

instance RunMessage AdministrationBuilding where
  runMessage msg l@(AdministrationBuilding attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeLocation_ $ SetAsideCardMatch $ CardWithTitle "Faculty Offices"
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discardTopOfDeck iid (attrs.ability 2) 1
      pure l
    _ -> AdministrationBuilding <$> liftRunMessage msg attrs
