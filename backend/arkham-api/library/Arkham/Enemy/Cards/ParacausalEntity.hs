module Arkham.Enemy.Cards.ParacausalEntity (paracausalEntity) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype ParacausalEntity = ParacausalEntity EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

paracausalEntity :: EnemyCard ParacausalEntity
paracausalEntity = enemy ParacausalEntity Cards.paracausalEntity (3, Static 2, 3) (1, 1)

instance HasAbilities ParacausalEntity where
  getAbilities (ParacausalEntity a) = extend1 a $ forcedAbility a 1 $ EnemyEngaged #when You (be a)

instance RunMessage ParacausalEntity where
  runMessage msg e@(ParacausalEntity attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lookAt iid (attrs.ability 1) iid [fromTopOfDeck 1] #any (defer attrs IsNotDraw)
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      focusCards cards do
        for_ cards \card -> do
          when (cardMatch card NonWeakness) $ hollow iid card
      pure e
    _ -> ParacausalEntity <$> liftRunMessage msg attrs
