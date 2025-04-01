module Arkham.Enemy.Cards.BenignElderThing (benignElderThing) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Matcher
import Arkham.Token

newtype BenignElderThing = BenignElderThing EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

benignElderThing :: EnemyCard BenignElderThing
benignElderThing =
  enemyWith BenignElderThing Cards.benignElderThing (1, Static 1, 1) (1, 1)
    $ (spawnAtL ?~ SpawnAt EmptyLocation)
    . (tokensL %~ setTokens #doom 1)

instance HasAbilities BenignElderThing where
  getAbilities (BenignElderThing a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyDefeated #when Anyone ByAny (be a)
      , skillTestAbility $ restricted a 2 OnSameLocation parleyAction_
      ]

instance RunMessage BenignElderThing where
  runMessage msg e@(BenignElderThing attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeDoomOnAgenda 1
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 2) iid #willpower (Fixed 3)
      pure e
    PassedThisSkillTest _iid (isAbilitySource attrs 2 -> True) -> do
      removeFromGame attrs
      pure e
    _ -> BenignElderThing <$> liftRunMessage msg attrs
