module Arkham.Location.Cards.UnderwaterCavern (underwaterCavern, UnderwaterCavern (..)) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Cave))

newtype UnderwaterCavern = UnderwaterCavern LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underwaterCavern :: LocationCard UnderwaterCavern
underwaterCavern = locationWith UnderwaterCavern Cards.underwaterCavern 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities UnderwaterCavern where
  getAbilities (UnderwaterCavern attrs) =
    extendRevealed
      attrs
      [ restrictedAbility
          attrs
          1
          (Here <> exists (not_ (be attrs) <> FloodedLocation <> LocationWithTrait Cave))
          $ ActionAbility [#move] (ActionCost 1)
      , mkAbility attrs 2 $ forced $ RevealLocation #after Anyone (be attrs)
      ]

instance RunMessage UnderwaterCavern where
  runMessage msg l@(UnderwaterCavern attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ls <- select $ not_ (be attrs) <> FloodedLocation <> LocationWithTrait Cave
      chooseTargetM iid ls $ moveTo (attrs.ability 1) iid
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      pure $ UnderwaterCavern $ attrs & floodLevelL ?~ FullyFlooded
    _ -> UnderwaterCavern <$> liftRunMessage msg attrs
