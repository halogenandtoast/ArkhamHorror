module Arkham.Enemy.Cards.HarlanEarnstoneCrazedByTheCurse (
  harlanEarnstoneCrazedByTheCurse,
  HarlanEarnstoneCrazedByTheCurse (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype HarlanEarnstoneCrazedByTheCurse = HarlanEarnstoneCrazedByTheCurse EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

harlanEarnstoneCrazedByTheCurse :: EnemyCard HarlanEarnstoneCrazedByTheCurse
harlanEarnstoneCrazedByTheCurse =
  enemy
    HarlanEarnstoneCrazedByTheCurse
    Cards.harlanEarnstoneCrazedByTheCurse
    (4, Static 2, 3)
    (1, 1)

instance HasAbilities HarlanEarnstoneCrazedByTheCurse where
  getAbilities (HarlanEarnstoneCrazedByTheCurse a) =
    withBaseAbilities
      a
      [ mkAbility a 1 $
          ForcedAbility $
            SkillTestResult
              Timing.After
              You
              (WhileEvadingAnEnemy $ EnemyWithId $ toId a)
              (SuccessResult $ AtLeast $ Static 3)
      ]

instance RunMessage HarlanEarnstoneCrazedByTheCurse where
  runMessage msg e@(HarlanEarnstoneCrazedByTheCurse attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ AddToVictory (toTarget attrs)
      pure e
    _ -> HarlanEarnstoneCrazedByTheCurse <$> runMessage msg attrs
