module Arkham.Enemy.Cards.BertieMusgrave (bertieMusgrave) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (codex, pattern Omega)
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Resident))
import Arkham.Window qualified as Window

newtype BertieMusgrave = BertieMusgrave EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bertieMusgrave :: EnemyCard BertieMusgrave
bertieMusgrave = enemy BertieMusgrave Cards.bertieMusgrave (3, Static 3, 3) (1, 0)

getWouldDamageEnemy :: [Window.Window] -> Maybe (Source, EnemyId, Int)
getWouldDamageEnemy =
  listToMaybe . mapMaybe \case
    (Window.windowType -> Window.WouldTakeDamage source (EnemyTarget eid) n _) -> Just (source, eid, n)
    _ -> Nothing

instance HasAbilities BertieMusgrave where
  getAbilities (BertieMusgrave a) =
    extend
      a
      $ [ mkAbility a 1 $ forced $ EnemyDefeated #when Anyone ByAny (be a)
        ]
      <> [ mkAbility a 2
             $ forced
             $ EnemyWouldTakeDamage
               #when
               AnySource
               (EnemyAt (locationWithEnemy a) <> EnemyWithTrait Resident <> not_ (be a))
         | not a.exhausted
         ]

instance RunMessage BertieMusgrave where
  runMessage msg e@(BertieMusgrave attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid (attrs.ability 1) Omega
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      for_ (getWouldDamageEnemy ws) \(_source, eid, n) -> do
        damageModifier (attrs.ability 2) eid (DamageTaken (-n))
        nonAttackEnemyDamage Nothing (attrs.ability 2) n attrs
      pure e
    _ -> BertieMusgrave <$> liftRunMessage msg attrs
