module Arkham.Enemy.Cards.ChildrenOfBlood.BloodMoney.HowardWilkesFirstChildOfZburamoarte (howardWilkesFirstChildOfZburamoarte) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype HowardWilkesFirstChildOfZburamoarte = HowardWilkesFirstChildOfZburamoarte EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

howardWilkesFirstChildOfZburamoarte :: EnemyCard HowardWilkesFirstChildOfZburamoarte
howardWilkesFirstChildOfZburamoarte = enemy HowardWilkesFirstChildOfZburamoarte Cards.howardWilkesFirstChildOfZburamoarte

instance HasModifiersFor HowardWilkesFirstChildOfZburamoarte where
  getModifiersFor (HowardWilkesFirstChildOfZburamoarte a) = do
    n <- getPlayerCount
    modifySelf a [HealthModifier (2 * n)]

instance HasAbilities HowardWilkesFirstChildOfZburamoarte where
  getAbilities (HowardWilkesFirstChildOfZburamoarte a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyTakeDamage #after AnyDamageEffect (be a) (atLeast 1) AnySource

instance RunMessage HowardWilkesFirstChildOfZburamoarte where
  runMessage msg e@(HowardWilkesFirstChildOfZburamoarte attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      if attrs.exhausted
        then ready attrs
        else selectEach (investigatorAt (locationWithEnemy attrs.id)) \iid -> chooseOneM iid $ withI18n do
          countVar 1 $ labeled' "takeDamage" $ assignDamage iid (attrs.ability 1) 1
          countVar 1 $ labeled' "takeHorror" $ assignHorror iid (attrs.ability 1) 1
      pure e
    _ -> HowardWilkesFirstChildOfZburamoarte <$> liftRunMessage msg attrs
