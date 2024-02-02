module Arkham.Location.Cards.EnchantedWoodsLostWoods (
  enchantedWoodsLostWoods,
  enchantedWoodsLostWoodsEffect,
  EnchantedWoodsLostWoods (..),
)
where

import Arkham.Effect.Runner hiding (RevealLocation)
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Movement
import Arkham.Prelude

newtype EnchantedWoodsLostWoods = EnchantedWoodsLostWoods LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

enchantedWoodsLostWoods :: LocationCard EnchantedWoodsLostWoods
enchantedWoodsLostWoods = location EnchantedWoodsLostWoods Cards.enchantedWoodsLostWoods 4 (PerPlayer 1)

instance HasAbilities EnchantedWoodsLostWoods where
  getAbilities (EnchantedWoodsLostWoods attrs) =
    withRevealedAbilities attrs
      $ [ mkAbility attrs 1
            $ ForcedAbility (RevealLocation #after You $ LocationWithId $ toId attrs)
        ]

instance RunMessage EnchantedWoodsLostWoods where
  runMessage msg l@(EnchantedWoodsLostWoods attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ createCardEffect Cards.enchantedWoodsLostWoods Nothing (toAbilitySource attrs 1) iid
      pure l
    _ -> EnchantedWoodsLostWoods <$> runMessage msg attrs

newtype EnchantedWoodsLostWoodsEffect = EnchantedWoodsLostWoodsEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

enchantedWoodsLostWoodsEffect :: EffectArgs -> EnchantedWoodsLostWoodsEffect
enchantedWoodsLostWoodsEffect = cardEffect EnchantedWoodsLostWoodsEffect Cards.enchantedWoodsLostWoods

instance RunMessage EnchantedWoodsLostWoodsEffect where
  runMessage msg e@(EnchantedWoodsLostWoodsEffect attrs) = case msg of
    EndRoundWindow -> do
      pushAll [disable attrs, PlaceDoomOnAgenda]
      pure e
    MoveTo (moveTarget -> InvestigatorTarget iid) | attrs.target == toTarget iid -> do
      push $ disable attrs
      pure e
    _ -> EnchantedWoodsLostWoodsEffect <$> runMessage msg attrs
