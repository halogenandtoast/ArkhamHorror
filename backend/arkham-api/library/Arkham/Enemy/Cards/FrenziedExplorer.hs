module Arkham.Enemy.Cards.FrenziedExplorer (frenziedExplorer) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Enemy.Types (Field (EnemyLocation))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Token
import Arkham.Window qualified as Window

newtype FrenziedExplorer = FrenziedExplorer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frenziedExplorer :: EnemyCard FrenziedExplorer
frenziedExplorer =
  enemyWith FrenziedExplorer Cards.frenziedExplorer (2, Static 2, 2) (1, 1)
    $ (tokensL %~ setTokens #doom 1)

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
      mlocation <- field EnemyLocation attrs.id
      for_ mlocation \location -> do
        moveTokens (attrs.ability 1) attrs location #doom (attrs.token #doom)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      when (attrs.token #doom > 0) do
        push $ FlipDoom (toTarget attrs) 1
        moveTokens (attrs.ability 2) attrs iid #clue 1
        checkAfter $ Window.TakeControlOfClues iid (toSource attrs) 1
        doStep 2 msg
      pure e
    DoStep 2 (UseThisAbility iid (isSource attrs -> True) 2) -> do
      when (attrs.token #doom == 0) $ toDiscardBy iid (attrs.ability 2) attrs
      pure e
    _ -> FrenziedExplorer <$> liftRunMessage msg attrs
