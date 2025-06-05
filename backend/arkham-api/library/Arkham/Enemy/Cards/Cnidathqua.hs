module Arkham.Enemy.Cards.Cnidathqua (cnidathqua) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype Cnidathqua = Cnidathqua EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cnidathqua :: EnemyCard Cnidathqua
cnidathqua =
  enemyWith Cnidathqua Cards.cnidathqua (4, PerPlayer 8, 0) (2, 2)
    $ (asSelfLocationL ?~ "cnidathqua")
    . (evadeL .~ Nothing)

instance HasModifiersFor Cnidathqua where
  getModifiersFor (Cnidathqua attrs) =
    modifySelf attrs [CannotBeEvaded, CanBeFoughtAsIfAtYourLocation]

instance HasAbilities Cnidathqua where
  getAbilities (Cnidathqua a) =
    extend
      a
      [ mkAbility a 1 $ forced $ SkillTestResult #after You (WhileAttackingAnEnemy $ be a) #failure
      , mkAbility a 2 $ Objective $ forced $ EnemyDefeated #when Anyone ByAny (be a)
      ]

instance RunMessage Cnidathqua where
  runMessage msg e@(Cnidathqua attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      findEncounterCard iid attrs (CardWithTitle "Writhing Appendage")
      pure e
    FoundEncounterCard iid (isTarget attrs -> True) card -> do
      lid <- getJustLocation iid
      push $ SpawnEnemyAtEngagedWith (EncounterCard card) lid iid
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R2
      pure e
    _ -> Cnidathqua <$> liftRunMessage msg attrs
