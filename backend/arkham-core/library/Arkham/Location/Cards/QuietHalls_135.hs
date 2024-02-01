module Arkham.Location.Cards.QuietHalls_135 (
  quietHalls_135,
  QuietHalls_135 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait

newtype QuietHalls_135 = QuietHalls_135 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

quietHalls_135 :: LocationCard QuietHalls_135
quietHalls_135 =
  locationWith
    QuietHalls_135
    Cards.quietHalls_135
    3
    (Static 0)
    ( (connectedMatchersL <>~ [LocationWithTrait ThirdFloor])
        . (revealedConnectedMatchersL <>~ [LocationWithTrait ThirdFloor])
    )

instance HasAbilities QuietHalls_135 where
  getAbilities (QuietHalls_135 attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility
          attrs
          1
          ( Here
              <> Negate (LocationExists UnrevealedLocation)
              <> Negate
                (LocationExists LocationWithAnyClues)
          )
          $ ActionAbility []
          $ ActionCost 1
      ]

instance RunMessage QuietHalls_135 where
  runMessage msg l@(QuietHalls_135 attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      amount <- getPlayerCount
      push $ PlaceClues (toAbilitySource attrs 1) (toTarget attrs) amount
      pure l
    _ -> QuietHalls_135 <$> runMessage msg attrs
