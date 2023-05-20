module Arkham.Location.Cards.VelmasDiner (
  velmasDiner,
  VelmasDiner (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype VelmasDiner = VelmasDiner LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

velmasDiner :: LocationCard VelmasDiner
velmasDiner = location VelmasDiner Cards.velmasDiner 2 (Static 0)

instance HasModifiersFor VelmasDiner where
  getModifiersFor (LocationTarget lid) (VelmasDiner a) = do
    isEasttown <- lid <=~> locationIs Cards.easttown
    pure $
      toModifiers
        a
        [ ConnectedToWhen (LocationWithId lid) (LocationWithId $ toId a)
        | isEasttown
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities VelmasDiner where
  getAbilities (VelmasDiner attrs) =
    withRevealedAbilities
      attrs
      [ limitedAbility (PlayerLimit PerGame 1) $
          restrictedAbility attrs 1 Here $
            ActionAbility Nothing $
              ActionCost 3
      ]

instance RunMessage VelmasDiner where
  runMessage msg l@(VelmasDiner attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ GainClues iid (toAbilitySource attrs 1) 2
      pure l
    _ -> VelmasDiner <$> runMessage msg attrs
