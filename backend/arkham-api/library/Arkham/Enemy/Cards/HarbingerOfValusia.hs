module Arkham.Enemy.Cards.HarbingerOfValusia (harbingerOfValusia) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Token

newtype HarbingerOfValusia = HarbingerOfValusia EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

harbingerOfValusia :: EnemyCard HarbingerOfValusia
harbingerOfValusia =
  enemy HarbingerOfValusia Cards.harbingerOfValusia (3, PerPlayer 10, 3) (2, 2)

instance HasModifiersFor HarbingerOfValusia where
  getModifiersFor (HarbingerOfValusia a) = modifySelf a [CanRetaliateWhileExhausted]

instance HasAbilities HarbingerOfValusia where
  getAbilities (HarbingerOfValusia a) =
    extend1 a
      $ playerLimit PerTestOrAbility
      $ mkAbility a 1
      $ forced
      $ oneOf
        [ SkillTestResult #after You (WhileEvadingAnEnemy $ be a) #success
        , SkillTestResult #after You (WhileAttackingAnEnemy $ be a) #success
        ]

instance RunMessage HarbingerOfValusia where
  runMessage msg e@(HarbingerOfValusia attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- getPlayerCountValue (PerPlayer 2)
      if attrs.resources + 1 >= n
        then do
          place attrs (OutOfPlay SetAsideZone)
          pure
            $ overAttrs (tokensL %~ setTokens #damage attrs.damage)
            $ cbCardBuilder harbingerOfValusia attrs.cardId attrs.id
        else do
          placeTokens (attrs.ability 1) attrs #resource 1
          pure e
    _ -> HarbingerOfValusia <$> liftRunMessage msg attrs
