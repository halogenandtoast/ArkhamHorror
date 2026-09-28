module Arkham.Homebrew.CircusExMortis.Acts.TheTrueMonster (theTrueMonster) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.CircusExMortis.Helpers (moonToken, sealMoonTokenOnTarget)
import Arkham.Matcher

newtype TheTrueMonster = TheTrueMonster ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueMonster :: ActCard TheTrueMonster
theTrueMonster = act (3, A) TheTrueMonster Cards.theTrueMonster Nothing

theBlackGoat :: EnemyMatcher
theBlackGoat = enemyIs Enemies.theBlackGoat

instance HasAbilities TheTrueMonster where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (exists moonToken <> exists theBlackGoat) $ FastAbility (ClueCost $ Static 1)
    , mkAbility a 2 $ Objective $ forced $ ifEnemyDefeatedMatch theBlackGoat
    ]

instance RunMessage TheTrueMonster where
  runMessage msg a@(TheTrueMonster attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sealMoonTokenOnTarget iid =<< selectJust theBlackGoat
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R4
      pure a
    _ -> TheTrueMonster <$> liftRunMessage msg attrs
