module Arkham.Location.Cards.RivertownAbandonedWarehouse (rivertownAbandonedWarehouse) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (rivertownAbandonedWarehouse)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait

newtype RivertownAbandonedWarehouse = RivertownAbandonedWarehouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rivertownAbandonedWarehouse :: LocationCard RivertownAbandonedWarehouse
rivertownAbandonedWarehouse =
  location RivertownAbandonedWarehouse Cards.rivertownAbandonedWarehouse 4 (PerPlayer 1)

instance HasAbilities RivertownAbandonedWarehouse where
  getAbilities (RivertownAbandonedWarehouse a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ actionAbilityWithCost (HandDiscardCost 1 $ basic $ CardWithSkillIcon #willpower)

willpowerCount :: Payment -> Int
willpowerCount (DiscardCardPayment cards) = sum $ map (count (== #willpower) . cdSkills . toCardDef) cards
willpowerCount (Payments xs) = sum $ map willpowerCount xs
willpowerCount _ = 0

instance RunMessage RivertownAbandonedWarehouse where
  runMessage msg l@(RivertownAbandonedWarehouse attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ payments -> do
      let doomToRemove = willpowerCount payments
      cultists <- select $ EnemyWithTrait Cultist
      chooseTargetM iid cultists \eid -> removeDoom (attrs.ability 1) eid doomToRemove
      pure l
    _ -> RivertownAbandonedWarehouse <$> liftRunMessage msg attrs
