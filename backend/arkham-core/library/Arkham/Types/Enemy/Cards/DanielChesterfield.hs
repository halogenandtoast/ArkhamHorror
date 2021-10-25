module Arkham.Types.Enemy.Cards.DanielChesterfield
  ( danielChesterfield
  , DanielChesterfield(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.SkillType

newtype DanielChesterfield = DanielChesterfield EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

danielChesterfield :: EnemyCard DanielChesterfield
danielChesterfield = enemyWith
  DanielChesterfield
  Cards.danielChesterfield
  (3, Static 4, 3)
  (1, 1)
  (preyL .~ HighestSkill SkillCombat)

instance HasAbilities DanielChesterfield where
  getAbilities (DanielChesterfield x) = withBaseAbilities
    x
    [ restrictedAbility
          x
          1
          (OnSameLocation <> AssetExists
            (AssetOwnedBy You <> assetIs Assets.claspOfBlackOnyx)
          )
        $ ActionAbility (Just Action.Parley) (ActionCost 1)
    ]

instance EnemyRunner env => RunMessage env DanielChesterfield where
  runMessage msg a@(DanielChesterfield attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AddToVictory $ toTarget attrs)
    _ -> DanielChesterfield <$> runMessage msg attrs
