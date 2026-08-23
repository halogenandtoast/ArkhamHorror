module Arkham.Enemy.Cards.ChildrenOfBlood.BloodMoney.PriscillaThomas (priscillaThomas) where

import Arkham.Ability
import Arkham.Campaigns.ChildrenOfBlood.Helpers
import Arkham.Enemy.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype PriscillaThomas = PriscillaThomas EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

priscillaThomas :: EnemyCard PriscillaThomas
priscillaThomas = enemy PriscillaThomas Cards.priscillaThomas

instance HasAbilities PriscillaThomas where
  getAbilities (PriscillaThomas a) =
    extend
      a
      [ restricted a 1 OnSameLocation
          $ ActionAbility #parley Nothing (ActionCost 1 <> ClueCost (PerPlayer 1))
      , mkAbility a 2 $ forced $ EnemyDefeated #after Anyone ByAny (be a)
      ]

instance RunMessage PriscillaThomas where
  runMessage msg e@(PriscillaThomas attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid (attrs.ability 1) 2
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      removeFromGame attrs
      pure e
    _ -> PriscillaThomas <$> liftRunMessage msg attrs
