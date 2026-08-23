module Arkham.Enemy.Cards.ChildrenOfBlood.BloodMoney.HowardWilkesTooFarGone (howardWilkesTooFarGone) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Matcher

newtype HowardWilkesTooFarGone = HowardWilkesTooFarGone EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

howardWilkesTooFarGone :: EnemyCard HowardWilkesTooFarGone
howardWilkesTooFarGone = enemy HowardWilkesTooFarGone Cards.howardWilkesTooFarGone

instance HasModifiersFor HowardWilkesTooFarGone where
  getModifiersFor (HowardWilkesTooFarGone a) = do
    n <- getPlayerCount
    modifySelf a [HealthModifier (2 * n)]

instance HasAbilities HowardWilkesTooFarGone where
  getAbilities (HowardWilkesTooFarGone a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyTakeDamage #after AnyDamageEffect (be a) (atLeast 1) AnySource

instance RunMessage HowardWilkesTooFarGone where
  runMessage msg e@(HowardWilkesTooFarGone attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      if attrs.exhausted
        then ready attrs
        else selectEach (investigatorAt (locationWithEnemy attrs.id)) \iid ->
          assignDamageAndHorror iid (attrs.ability 1) 1 1
      pure e
    _ -> HowardWilkesTooFarGone <$> liftRunMessage msg attrs
