module Arkham.Treachery.Cards.DimensionalBreach (dimensionalBreach) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Trait (Trait (Portal))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DimensionalBreach = DimensionalBreach TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalBreach :: TreacheryCard DimensionalBreach
dimensionalBreach = treachery DimensionalBreach Cards.dimensionalBreach

instance HasModifiersFor DimensionalBreach where
  getModifiersFor (DimensionalBreach a) = case a.attached of
    Just (LocationTarget lid) ->
      modifySelect a (EnemyAt $ LocationWithId lid) [EnemyFight 1, EnemyEvade 1, HealthModifier 1]
    _ -> pure mempty

instance HasAbilities DimensionalBreach where
  getAbilities (DimensionalBreach a) =
    [restricted a 1 OnSameLocation actionAbility]

instance RunMessage DimensionalBreach where
  runMessage msg t@(DimensionalBreach attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      locations <- select $ NearestLocationToYou $ LocationWithTrait Portal
      chooseOrRunOneM iid do
        targets locations \lid -> do
          attachTreachery attrs lid
          placeDoom attrs attrs 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> DimensionalBreach <$> liftRunMessage msg attrs
