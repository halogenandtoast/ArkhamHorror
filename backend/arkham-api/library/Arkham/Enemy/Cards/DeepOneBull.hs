module Arkham.Enemy.Cards.DeepOneBull (deepOneBull, DeepOneBull (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (DeepOne))

newtype DeepOneBull = DeepOneBull EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepOneBull :: EnemyCard DeepOneBull
deepOneBull = enemy DeepOneBull Cards.deepOneBull (4, Static 5, 2) (2, 0)

instance HasAbilities DeepOneBull where
  getAbilities (DeepOneBull a) =
    extend
      a
      [ forcedAbility a 1 $ EnemyEngaged #after You (be a)
      , forcedAbility a 2 $ EnemyDefeated #after investigator ByAny (not_ (be a) <> EnemyWithTrait DeepOne)
      ]
   where
    investigator = if a.exhausted then Anyone else at_ (not_ $ locationWithEnemy a.id)

instance RunMessage DeepOneBull where
  runMessage msg e@(DeepOneBull attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAndDiscardCard iid (attrs.ability 1)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      when attrs.exhausted $ ready attrs
      moveTowardsMatching (attrs.ability 2) attrs (locationWithInvestigator iid)
      pure e
    _ -> DeepOneBull <$> liftRunMessage msg attrs
