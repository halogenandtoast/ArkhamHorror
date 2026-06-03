module Arkham.Enemy.Cards.BertieMusgrave (bertieMusgrave) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (codex, pattern Omega)
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype BertieMusgrave = BertieMusgrave EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bertieMusgrave :: EnemyCard BertieMusgrave
bertieMusgrave = enemy BertieMusgrave Cards.bertieMusgrave (3, Static 3, 3) (1, 0)

-- TODO: "When damage would be assigned to a Resident enemy at Bertie Musgrave's
-- location and he is ready: It is assigned to Bertie Musgrave, instead." This is
-- a damage-assignment redirection that still needs a replacement effect.
instance HasAbilities BertieMusgrave where
  getAbilities (BertieMusgrave a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyDefeated #when Anyone ByAny (be a)

instance RunMessage BertieMusgrave where
  runMessage msg e@(BertieMusgrave attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid (attrs.ability 1) Omega
      pure e
    _ -> BertieMusgrave <$> liftRunMessage msg attrs
