module Arkham.Enemy.Cards.FrenziedExplorer (frenziedExplorer) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Token

newtype FrenziedExplorer = FrenziedExplorer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frenziedExplorer :: EnemyCard FrenziedExplorer
frenziedExplorer =
  enemyWith
    FrenziedExplorer
    Cards.frenziedExplorer
    (2, Static 2, 2)
    (1, 1)
    (tokensL %~ setTokens #doom 1)

instance HasAbilities FrenziedExplorer where
  getAbilities (FrenziedExplorer a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyDefeated #when Anyone ByAny (be a)
      , restricted a 2 criteria
          $ freeReaction
          $ SkillTestResult #after You (WhileEvadingAnEnemy $ be a) (SuccessResult $ atLeast 2)
      ]
   where
    criteria = if a.token #doom > 0 then NoRestriction else Never

instance RunMessage FrenziedExplorer where
  runMessage msg e@(FrenziedExplorer attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withLocationOf attrs $ moveTokensFrom (attrs.ability 1) attrs #doom (attrs.token #doom)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      when (attrs.token #doom > 0) do
        moveTokens (attrs.ability 2) attrs iid #doom 1
        flipDoomToClues iid 1
        doStep 2 msg
      pure e
    DoStep 2 (UseThisAbility iid (isSource attrs -> True) 2) -> do
      when (attrs.token #doom == 0) $ toDiscardBy iid (attrs.ability 2) attrs
      pure e
    _ -> FrenziedExplorer <$> liftRunMessage msg attrs
