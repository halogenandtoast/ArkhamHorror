module Arkham.Types.Location.Cards.SouthsideMasBoardingHouse
  ( SouthsideMasBoardingHouse(..)
  , southsideMasBoardingHouse
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (southsideMasBoardingHouse)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target

newtype SouthsideMasBoardingHouse = SouthsideMasBoardingHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southsideMasBoardingHouse :: LocationCard SouthsideMasBoardingHouse
southsideMasBoardingHouse = location
  SouthsideMasBoardingHouse
  Cards.southsideMasBoardingHouse
  2
  (PerPlayer 1)
  Square
  [Diamond, Plus, Circle]

instance HasAbilities SouthsideMasBoardingHouse where
  getAbilities (SouthsideMasBoardingHouse x) | locationRevealed x =
    withBaseAbilities x
      $ [ restrictedAbility x 1 Here (ActionAbility Nothing $ ActionCost 1)
          & abilityLimitL
          .~ PlayerLimit PerGame 1
        ]
  getAbilities (SouthsideMasBoardingHouse attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env SouthsideMasBoardingHouse where
  runMessage msg l@(SouthsideMasBoardingHouse attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (Search iid source (InvestigatorTarget iid) [fromDeck] IsAlly
      $ DrawFound iid 1
      )
    _ -> SouthsideMasBoardingHouse <$> runMessage msg attrs
