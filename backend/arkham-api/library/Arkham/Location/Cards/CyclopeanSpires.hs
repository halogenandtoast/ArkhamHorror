module Arkham.Location.Cards.CyclopeanSpires (cyclopeanSpires) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.CityOfTheElderThings.Helpers

newtype CyclopeanSpires = CyclopeanSpires LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cyclopeanSpires :: LocationCard CyclopeanSpires
cyclopeanSpires = locationWith CyclopeanSpires Cards.cyclopeanSpires 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities CyclopeanSpires where
  getAbilities (CyclopeanSpires a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted
        a
        1
        (Here <> exists (not_ (be a) <> LocationWithAnyKeys <> LocationInRowOf (be a)))
        actionAbility

instance RunMessage CyclopeanSpires where
  runMessage msg l@(CyclopeanSpires attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ls <- select $ not_ (be attrs) <> LocationWithAnyKeys <> LocationInRowOf (be attrs)
      chooseTargetM iid ls (handleTarget iid (attrs.ability 1))
      pure l
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (LocationTarget lid) -> do
      ks <- field LocationKeys lid
      chooseOneM iid do
        for_ ks \k -> scenarioI18n $ keyVar "key" (keyName k) $ labeled' "moveKey" do
          push $ PlaceKey (toTarget attrs) k
      pure l
    _ -> CyclopeanSpires <$> liftRunMessage msg attrs
