module Arkham.Location.Cards.GilmanHouseInTooDeep (
  gilmanHouseInTooDeep,
  GilmanHouseInTooDeep (..),
)
where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.InTooDeep.Helpers

newtype GilmanHouseInTooDeep = GilmanHouseInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gilmanHouseInTooDeep :: LocationCard GilmanHouseInTooDeep
gilmanHouseInTooDeep = locationWith GilmanHouseInTooDeep Cards.gilmanHouseInTooDeep 3 (PerPlayer 2) connectsToAdjacent

instance HasAbilities GilmanHouseInTooDeep where
  getAbilities (GilmanHouseInTooDeep a) =
    extendRevealed
      a
      [ restricted a 1 UnrevealedKeyIsSetAside $ forced $ RevealLocation #after Anyone (be a)
      , skillTestAbility $ restricted a 2 Here parleyAction_
      ]

instance RunMessage GilmanHouseInTooDeep where
  runMessage msg l@(GilmanHouseInTooDeep attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeUnrevealedKeyOn attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#combat, #agility] \skill ->
          skillLabeled skill $ beginSkillTest sid iid (attrs.ability 2) iid skill (Fixed 2)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      removeBarrierBetweenConnected iid attrs.id
      pure l
    _ -> GilmanHouseInTooDeep <$> liftRunMessage msg attrs
