module Arkham.Enemy.Cards.HarbingerOfValusiaTheSleeperReturns (harbingerOfValusiaTheSleeperReturns) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types qualified as Field
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Projection
import Arkham.Token

newtype HarbingerOfValusiaTheSleeperReturns = HarbingerOfValusiaTheSleeperReturns EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

harbingerOfValusiaTheSleeperReturns :: EnemyCard HarbingerOfValusiaTheSleeperReturns
harbingerOfValusiaTheSleeperReturns =
  enemy
    HarbingerOfValusiaTheSleeperReturns
    Cards.harbingerOfValusiaTheSleeperReturns
    (2, PerPlayer 10, 4)
    (2, 2)

instance HasModifiersFor HarbingerOfValusiaTheSleeperReturns where
  getModifiersFor (HarbingerOfValusiaTheSleeperReturns a) = do
    dmg <- field Field.EnemyDamage a.id
    n <- perPlayer 5
    when (dmg >= n) $ modifySelf a [EnemyFight 2, EnemyEvade (-2)]

instance HasAbilities HarbingerOfValusiaTheSleeperReturns where
  getAbilities (HarbingerOfValusiaTheSleeperReturns a) =
    extend1 a
      $ playerLimit PerTestOrAbility
      $ mkAbility a 1
      $ forced
      $ oneOf
        [ SkillTestResult #after You (WhileEvadingAnEnemy $ be a) #success
        , SkillTestResult #after You (WhileAttackingAnEnemy $ be a) #success
        ]

instance RunMessage HarbingerOfValusiaTheSleeperReturns where
  runMessage msg e@(HarbingerOfValusiaTheSleeperReturns attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- getPlayerCountValue (PerPlayer 2)
      if attrs.resources + 1 >= n
        then do
          place attrs (OutOfPlay SetAsideZone)
          pure
            $ overAttrs (tokensL %~ setTokens #damage attrs.damage)
            $ cbCardBuilder harbingerOfValusiaTheSleeperReturns attrs.cardId attrs.id
        else do
          placeTokens (attrs.ability 1) attrs #resource 1
          pure e
    _ -> HarbingerOfValusiaTheSleeperReturns <$> liftRunMessage msg attrs
