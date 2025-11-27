module Arkham.Enemy.Cards.VoidChimeraGorefeaster (voidChimeraGorefeaster) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Strategy

newtype VoidChimeraGorefeaster = VoidChimeraGorefeaster EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

voidChimeraGorefeaster :: EnemyCard VoidChimeraGorefeaster
voidChimeraGorefeaster = enemy VoidChimeraGorefeaster Cards.voidChimeraGorefeaster (3, Static 4, 3) (1, 2)

instance HasModifiersFor VoidChimeraGorefeaster where
  getModifiersFor (VoidChimeraGorefeaster a) = do
    n <- perPlayer 1
    modifySelf a [HealthModifier n]

instance HasAbilities VoidChimeraGorefeaster where
  getAbilities (VoidChimeraGorefeaster x) =
    extend1 x $ mkAbility x 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be x)

instance RunMessage VoidChimeraGorefeaster where
  runMessage msg e@(VoidChimeraGorefeaster attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lookAt iid (attrs.ability 1) iid [fromTopOfDeck 1] #any (defer attrs IsNotDraw)
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      focusCards cards do
        for_ cards \card -> do
          when (cardMatch card NonWeakness) $ hollow iid card
          healDamage attrs (attrs.ability 1) card.printedCost
      pure e
    _ -> VoidChimeraGorefeaster <$> liftRunMessage msg attrs
