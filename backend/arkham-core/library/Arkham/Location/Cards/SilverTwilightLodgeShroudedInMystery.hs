module Arkham.Location.Cards.SilverTwilightLodgeShroudedInMystery (
  silverTwilightLodgeShroudedInMystery,
  SilverTwilightLodgeShroudedInMystery (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype SilverTwilightLodgeShroudedInMystery = SilverTwilightLodgeShroudedInMystery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

silverTwilightLodgeShroudedInMystery :: LocationCard SilverTwilightLodgeShroudedInMystery
silverTwilightLodgeShroudedInMystery =
  location
    SilverTwilightLodgeShroudedInMystery
    Cards.silverTwilightLodgeShroudedInMystery
    4
    (PerPlayer 1)

instance HasAbilities SilverTwilightLodgeShroudedInMystery where
  getAbilities (SilverTwilightLodgeShroudedInMystery attrs) =
    withRevealedAbilities
      attrs
      [fastAbility attrs 1 (HorrorCost (toAbilitySource attrs 1) YouTarget 1) Here]

instance RunMessage SilverTwilightLodgeShroudedInMystery where
  runMessage msg l@(SilverTwilightLodgeShroudedInMystery attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      act <- selectJust AnyAct
      pushAll [RemoveBreaches (toTarget attrs) 1, PlaceBreaches (toTarget act) 1]
      pure l
    _ -> SilverTwilightLodgeShroudedInMystery <$> runMessage msg attrs
