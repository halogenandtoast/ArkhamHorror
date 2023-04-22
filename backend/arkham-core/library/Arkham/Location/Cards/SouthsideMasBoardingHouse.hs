module Arkham.Location.Cards.SouthsideMasBoardingHouse
  ( SouthsideMasBoardingHouse(..)
  , southsideMasBoardingHouse
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( southsideMasBoardingHouse )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher

newtype SouthsideMasBoardingHouse = SouthsideMasBoardingHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southsideMasBoardingHouse :: LocationCard SouthsideMasBoardingHouse
southsideMasBoardingHouse = location
  SouthsideMasBoardingHouse
  Cards.southsideMasBoardingHouse
  2
  (PerPlayer 1)

instance HasAbilities SouthsideMasBoardingHouse where
  getAbilities (SouthsideMasBoardingHouse x) | locationRevealed x =
    withBaseAbilities x
      $ [ limitedAbility (PlayerLimit PerGame 1)
            $ restrictedAbility x 1 Here (ActionAbility Nothing $ ActionCost 1)
        ]
  getAbilities (SouthsideMasBoardingHouse attrs) = getAbilities attrs

instance RunMessage SouthsideMasBoardingHouse where
  runMessage msg l@(SouthsideMasBoardingHouse attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> l <$ push
      (Search iid source (InvestigatorTarget iid) [fromDeck] IsAlly
      $ DrawFound iid 1
      )
    _ -> SouthsideMasBoardingHouse <$> runMessage msg attrs
