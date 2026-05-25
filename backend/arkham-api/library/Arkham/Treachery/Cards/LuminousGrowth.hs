module Arkham.Treachery.Cards.LuminousGrowth (luminousGrowth) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_, modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Trait (Trait (Dark))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LuminousGrowth = LuminousGrowth TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luminousGrowth :: TreacheryCard LuminousGrowth
luminousGrowth = treachery LuminousGrowth Cards.luminousGrowth

instance HasModifiersFor LuminousGrowth where
  getModifiersFor (LuminousGrowth attrs) = case attrs.placement of
    AttachedToLocation lid -> do
      modified_ attrs lid [RemoveTrait Dark]
      modifySelect attrs (EnemyAt $ LocationWithId lid) [EnemyFight 1, DamageDealt 1]
    _ -> pure mempty

instance HasAbilities LuminousGrowth where
  getAbilities (LuminousGrowth a) =
    [skillTestAbility $ restricted a 1 OnSameLocation actionAbility]

instance RunMessage LuminousGrowth where
  runMessage msg t@(LuminousGrowth attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      ls <-
        select
          $ NearestLocationTo iid
          $ RevealedLocation
          <> LocationWithTrait Dark
          <> LocationWithoutTreachery (treacheryIs Cards.luminousGrowth)
      chooseTargetM iid ls $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> LuminousGrowth <$> liftRunMessage msg attrs
