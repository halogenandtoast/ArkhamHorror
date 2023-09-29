module Arkham.Enemy.Cards.NathanWickMasterOfIndoctrination (
  nathanWickMasterOfIndoctrination,
  NathanWickMasterOfIndoctrination (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype NathanWickMasterOfIndoctrination = NathanWickMasterOfIndoctrination EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nathanWickMasterOfIndoctrination :: EnemyCard NathanWickMasterOfIndoctrination
nathanWickMasterOfIndoctrination =
  enemy
    NathanWickMasterOfIndoctrination
    Cards.nathanWickMasterOfIndoctrination
    (4, Static 5, 3)
    (1, 1)

instance HasAbilities NathanWickMasterOfIndoctrination where
  getAbilities (NathanWickMasterOfIndoctrination attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
          $ ForcedAbility
          $ SkillTestResult
            Timing.After
            You
            (WhileEvadingAnEnemy $ EnemyWithId $ toId attrs)
            (SuccessResult $ AtLeast $ Static 3)
      ]

instance RunMessage NathanWickMasterOfIndoctrination where
  runMessage msg e@(NathanWickMasterOfIndoctrination attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ AddToVictory (toTarget attrs)
      pure e
    _ -> NathanWickMasterOfIndoctrination <$> runMessage msg attrs
