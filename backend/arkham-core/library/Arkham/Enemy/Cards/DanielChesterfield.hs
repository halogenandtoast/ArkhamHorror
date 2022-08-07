module Arkham.Enemy.Cards.DanielChesterfield
  ( danielChesterfield
  , DanielChesterfield(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType

newtype DanielChesterfield = DanielChesterfield EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

danielChesterfield :: EnemyCard DanielChesterfield
danielChesterfield = enemyWith
  DanielChesterfield
  Cards.danielChesterfield
  (3, Static 4, 3)
  (1, 1)
  (preyL .~ Prey (InvestigatorWithHighestSkill SkillCombat))

instance HasAbilities DanielChesterfield where
  getAbilities (DanielChesterfield x) = withBaseAbilities
    x
    [ restrictedAbility
          x
          1
          (OnSameLocation <> AssetExists
            (AssetControlledBy You <> assetIs Assets.claspOfBlackOnyx)
          )
        $ ActionAbility (Just Action.Parley) (ActionCost 1)
    ]

instance RunMessage DanielChesterfield where
  runMessage msg a@(DanielChesterfield attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AddToVictory $ toTarget attrs)
    _ -> DanielChesterfield <$> runMessage msg attrs
