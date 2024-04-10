module Arkham.Enemy.Cards.WebSpinner (webSpinner, WebSpinner (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype WebSpinner = WebSpinner EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

webSpinner :: EnemyCard WebSpinner
webSpinner =
  enemyWith WebSpinner Cards.webSpinner (2, Static 2, 2) (1, 1)
    $ spawnAtL
    ?~ SpawnAt EmptyLocation

instance HasAbilities WebSpinner where
  getAbilities (WebSpinner attrs) =
    extend
      attrs
      [restrictedAbility attrs 1 (exists $ be attrs <> ReadyEnemy) $ forced $ RoundEnds #when]

instance RunMessage WebSpinner where
  runMessage msg e@(WebSpinner attrs) = runQueueT case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      mLocation <- select $ locationWithEnemy attrs.id
      for_ mLocation \lid -> placeDoom (attrs.ability 1) lid 1
      pure e
    _ -> WebSpinner <$> lift (runMessage msg attrs)
