module Arkham.Location.Cards.SalemGaol1692 (salemGaol1692) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Move

newtype SalemGaol1692 = SalemGaol1692 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

salemGaol1692 :: LocationCard SalemGaol1692
salemGaol1692 = location SalemGaol1692 Cards.salemGaol1692 3 (PerPlayer 1)

instance HasAbilities SalemGaol1692 where
  getAbilities (SalemGaol1692 a) =
    extendRevealed
      a
      [ skillTestAbility $ playerLimit PerGame $ restricted a 1 Here actionAbility
      , haunted "Move to Keziah's Room." a 2
      ]

instance RunMessage SalemGaol1692 where
  runMessage msg l@(SalemGaol1692 attrs) = runQueueT $ case msg of
    Msg.RevealLocation _ lid | lid == toId attrs -> do
      SalemGaol1692 <$> liftRunMessage msg (attrs & labelL .~ "salemGaol1692")
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 3)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      moveToMatch attrs iid (locationIs Locations.keziahsRoom)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      moveToMatch attrs iid RevealedLocation
      pure l
    _ -> SalemGaol1692 <$> liftRunMessage msg attrs
