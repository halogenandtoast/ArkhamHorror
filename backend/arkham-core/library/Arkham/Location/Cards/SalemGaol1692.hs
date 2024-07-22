module Arkham.Location.Cards.SalemGaol1692 (
  salemGaol1692,
  SalemGaol1692 (..),
) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Movement
import Arkham.SkillType

newtype SalemGaol1692 = SalemGaol1692 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

salemGaol1692 :: LocationCard SalemGaol1692
salemGaol1692 = location SalemGaol1692 Cards.salemGaol1692 3 (PerPlayer 1)

instance HasAbilities SalemGaol1692 where
  getAbilities (SalemGaol1692 a) =
    withRevealedAbilities
      a
      [ limitedAbility (PlayerLimit PerGame 1)
          $ restrictedAbility a 1 Here
          $ ActionAbility []
          $ ActionCost 1
      , haunted "Move to Keziah's Room." a 2
      ]

instance RunMessage SalemGaol1692 where
  runMessage msg l@(SalemGaol1692 attrs) = case msg of
    Msg.RevealLocation _ lid | lid == toId attrs -> do
      SalemGaol1692 <$> runMessage msg (attrs & labelL .~ "salemGaol1692")
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (attrs.ability 1) iid SkillIntellect (Fixed 3)
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ Move $ moveToMatch attrs iid (locationIs Locations.keziahsRoom)
      pure l
    PassedSkillTest iid _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ _ -> do
      push $ Move $ moveToMatch attrs iid RevealedLocation
      pure l
    _ -> SalemGaol1692 <$> runMessage msg attrs
