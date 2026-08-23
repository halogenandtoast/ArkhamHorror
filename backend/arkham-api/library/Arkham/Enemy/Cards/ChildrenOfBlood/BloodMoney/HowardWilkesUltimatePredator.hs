module Arkham.Enemy.Cards.ChildrenOfBlood.BloodMoney.HowardWilkesUltimatePredator (howardWilkesUltimatePredator) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Matcher

newtype HowardWilkesUltimatePredator = HowardWilkesUltimatePredator EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

howardWilkesUltimatePredator :: EnemyCard HowardWilkesUltimatePredator
howardWilkesUltimatePredator = enemy HowardWilkesUltimatePredator Cards.howardWilkesUltimatePredator

instance HasModifiersFor HowardWilkesUltimatePredator where
  getModifiersFor (HowardWilkesUltimatePredator a) = do
    n <- getPlayerCount
    modifySelf a [HealthModifier (2 * n)]

instance HasAbilities HowardWilkesUltimatePredator where
  getAbilities (HowardWilkesUltimatePredator a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyTakeDamage #after AnyDamageEffect (be a) (atLeast 1) AnySource

instance RunMessage HowardWilkesUltimatePredator where
  runMessage msg e@(HowardWilkesUltimatePredator attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      ready attrs
      selectEach (investigatorAt (locationWithEnemy attrs.id)) \iid -> assignDamageAndHorror iid (attrs.ability 1) 1 1
      pure e
    _ -> HowardWilkesUltimatePredator <$> liftRunMessage msg attrs
