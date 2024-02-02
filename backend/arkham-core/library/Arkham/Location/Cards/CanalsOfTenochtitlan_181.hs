module Arkham.Location.Cards.CanalsOfTenochtitlan_181 (
  canalsOfTenochtitlan_181,
  CanalsOfTenochtitlan_181 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype CanalsOfTenochtitlan_181 = CanalsOfTenochtitlan_181 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

canalsOfTenochtitlan_181 :: LocationCard CanalsOfTenochtitlan_181
canalsOfTenochtitlan_181 =
  locationWith
    CanalsOfTenochtitlan_181
    Cards.canalsOfTenochtitlan_181
    2
    (PerPlayer 1)
    (labelL .~ "diamond")

instance HasModifiersFor CanalsOfTenochtitlan_181 where
  getModifiersFor target (CanalsOfTenochtitlan_181 a)
    | isTarget a target =
        pure
          $ toModifiers
            a
            [ShroudModifier (locationResources a) | locationResources a > 0]
  getModifiersFor _ _ = pure []

instance HasAbilities CanalsOfTenochtitlan_181 where
  getAbilities (CanalsOfTenochtitlan_181 attrs) =
    withRevealedAbilities attrs
      $ [ mkAbility attrs 1
            $ ForcedAbility
            $ PutLocationIntoPlay Timing.After Anyone
            $ LocationWithId
            $ toId attrs
        , restrictedAbility attrs 2 (ResourcesOnThis $ AtLeast $ Static 1)
            $ ForcedAbility
            $ RoundEnds Timing.When
        ]

instance RunMessage CanalsOfTenochtitlan_181 where
  runMessage msg l@(CanalsOfTenochtitlan_181 attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ PlaceResources (toAbilitySource attrs 1) (toTarget attrs) 4
      pure l
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ RemoveResources (toAbilitySource attrs 2) (toTarget attrs) 1
      pure l
    _ -> CanalsOfTenochtitlan_181 <$> runMessage msg attrs
