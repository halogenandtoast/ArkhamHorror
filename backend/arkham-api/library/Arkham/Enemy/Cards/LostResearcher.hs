module Arkham.Enemy.Cards.LostResearcher (lostResearcher) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Enemy.Types (Field (EnemyLocation))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Token
import Arkham.Window qualified as Window

newtype LostResearcher = LostResearcher EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostResearcher :: EnemyCard LostResearcher
lostResearcher =
  enemyWith LostResearcher Cards.lostResearcher (1, Static 1, 1) (1, 1)
    $ (spawnAtL ?~ SpawnAtFirst [SpawnAt EmptyLocation, SpawnAt Anywhere])
    . (tokensL %~ setTokens #doom 1)

instance HasAbilities LostResearcher where
  getAbilities (LostResearcher a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyDefeated #when Anyone ByAny (be a)
      , skillTestAbility $ restricted a 2 OnSameLocation parleyAction_
      ]

instance RunMessage LostResearcher where
  runMessage msg e@(LostResearcher attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      mlocation <- field EnemyLocation attrs.id
      for_ mlocation \location -> do
        moveTokens (attrs.ability 1) attrs location #doom (attrs.token #doom)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#willpower, #intellect] \sType ->
          skillLabeled sType $ beginSkillTest sid iid (attrs.ability 2) iid sType (Fixed 3)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      when (attrs.token #doom > 0) do
        push $ FlipDoom (toTarget attrs) 1
        moveTokens (attrs.ability 2) attrs iid #clue 1
        checkAfter $ Window.TakeControlOfClues iid (toSource attrs) 1
        doStep 2 msg
      pure e
    DoStep 2 (PassedThisSkillTest iid (isAbilitySource attrs 2 -> True)) -> do
      when (attrs.token #doom == 0) $ toDiscardBy iid (attrs.ability 2) attrs
      pure e
    _ -> LostResearcher <$> liftRunMessage msg attrs
