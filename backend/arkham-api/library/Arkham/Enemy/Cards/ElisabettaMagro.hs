module Arkham.Enemy.Cards.ElisabettaMagro (
  elisabettaMagro,
  ElisabettaMagro (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype ElisabettaMagro = ElisabettaMagro EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elisabettaMagro :: EnemyCard ElisabettaMagro
elisabettaMagro = enemy ElisabettaMagro Cards.elisabettaMagro (3, Static 4, 4) (1, 1)

{- | Abilities
The first forced ability is handled by MaskedCarnevaleGoer_18
-}
instance HasAbilities ElisabettaMagro where
  getAbilities (ElisabettaMagro attrs) =
    withBaseAbilities attrs [mkAbility attrs 1 $ ForcedAbility $ PhaseEnds #when #mythos]

instance RunMessage ElisabettaMagro where
  runMessage msg e@(ElisabettaMagro attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ placeDoom (toAbilitySource attrs 1) attrs 1
      pure e
    _ -> ElisabettaMagro <$> runMessage msg attrs
