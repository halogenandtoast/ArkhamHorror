module Arkham.Location.Cards.OsbornsGeneralStore_207 (
  osbornsGeneralStore_207,
  OsbornsGeneralStore_207 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (osbornsGeneralStore_207)
import Arkham.Location.Runner
import Arkham.Matcher

newtype OsbornsGeneralStore_207 = OsbornsGeneralStore_207 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

osbornsGeneralStore_207 :: LocationCard OsbornsGeneralStore_207
osbornsGeneralStore_207 =
  location OsbornsGeneralStore_207 Cards.osbornsGeneralStore_207 3 (PerPlayer 1)

instance HasAbilities OsbornsGeneralStore_207 where
  getAbilities (OsbornsGeneralStore_207 attrs) =
    let rest = withDrawCardUnderneathAction attrs
     in [restrictedAbility attrs 1 Here $ actionAbilityWithCost (ResourceCost 1) | attrs.revealed]
          <> rest

instance RunMessage OsbornsGeneralStore_207 where
  runMessage msg l@(OsbornsGeneralStore_207 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push
        $ search iid (attrs.ability 1) iid [fromTopOfDeck 3] (basic $ #asset <> #item) (DrawFound iid 1)
      pure l
    _ -> OsbornsGeneralStore_207 <$> runMessage msg attrs
