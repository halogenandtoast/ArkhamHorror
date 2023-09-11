module Arkham.Location.Cards.SouthsideMasBoardingHouse (
  SouthsideMasBoardingHouse (..),
  southsideMasBoardingHouse,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (southsideMasBoardingHouse)
import Arkham.Location.Runner
import Arkham.Matcher

newtype SouthsideMasBoardingHouse = SouthsideMasBoardingHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southsideMasBoardingHouse :: LocationCard SouthsideMasBoardingHouse
southsideMasBoardingHouse = location SouthsideMasBoardingHouse Cards.southsideMasBoardingHouse 2 (PerPlayer 1)

instance HasAbilities SouthsideMasBoardingHouse where
  getAbilities (SouthsideMasBoardingHouse x) =
    withRevealedAbilities x
      $ [ limitedAbility (PlayerLimit PerGame 1)
            $ restrictedAbility x 1 Here (ActionAbility Nothing $ ActionCost 1)
        ]

instance RunMessage SouthsideMasBoardingHouse where
  runMessage msg l@(SouthsideMasBoardingHouse attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push
        $ Search iid (toAbilitySource attrs 1) (toTarget iid) [fromDeck] IsAlly
        $ DrawFound iid 1
      pure l
    _ -> SouthsideMasBoardingHouse <$> runMessage msg attrs
