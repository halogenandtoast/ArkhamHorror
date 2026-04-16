module Arkham.Enemy.Cards.Poisonblossom (poisonblossom) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Token

newtype Poisonblossom = Poisonblossom EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

poisonblossom :: EnemyCard Poisonblossom
poisonblossom = enemy Poisonblossom Cards.poisonblossom (2, Static 3, 1) (0, 1)

instance HasModifiersFor Poisonblossom where
  getModifiersFor (Poisonblossom a) = do
    let overgrowth = a.token Overgrowth
    when (overgrowth > 0) do
      getCampaignTime >>= \case
        Day -> modifySelf a [HealthModifier overgrowth, DamageDealt 1]
        Night -> modifySelf a [EnemyFight 1, DamageDealt 1]

instance HasAbilities Poisonblossom where
  getAbilities (Poisonblossom a) = extend a [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage Poisonblossom where
  runMessage msg e@(Poisonblossom attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs Overgrowth 1
      pure e
    _ -> Poisonblossom <$> liftRunMessage msg attrs
