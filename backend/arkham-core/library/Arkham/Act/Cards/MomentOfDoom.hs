module Arkham.Act.Cards.MomentOfDoom (
  MomentOfDoom (..),
  momentOfDoom,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Ability
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message hiding (EnemyDefeated)
import Arkham.Projection
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype MomentOfDoom = MomentOfDoom ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

momentOfDoom :: ActCard MomentOfDoom
momentOfDoom = act (3, A) MomentOfDoom Cards.momentOfDoom Nothing

instance HasAbilities MomentOfDoom where
  getAbilities (MomentOfDoom attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility
          ( ProxySource
              (AssetMatcherSource $ assetIs Assets.relicOfAgesRepossessThePast)
              (toSource attrs)
          )
          1
          ControlsThis
          $ ActionAbility Nothing
          $ ActionCost 1
      , mkAbility attrs 2
          $ Objective
          $ ForcedAbility
          $ EnemyDefeated Timing.After Anyone ByAny
          $ enemyIs Enemies.yig
      ]

instance RunMessage MomentOfDoom where
  runMessage msg a@(MomentOfDoom attrs) = case msg of
    UseCardAbility iid (ProxySource _ (isSource attrs -> True)) 1 _ _ -> do
      push
        $ chooseOne
          iid
          [ SkillLabel
            skill
            [beginSkillTest iid attrs attrs skill 4]
          | skill <- [SkillWillpower, SkillIntellect]
          ]
      pure a
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ ->
      do
        mlid <- field InvestigatorLocation iid
        for_ mlid $ \lid -> do
          yig <- selectJust $ enemyIs Enemies.yig
          iids <- selectList $ colocatedWith iid <> InvestigatorWithAnyClues
          unless (null iids)
            $ push
            $ chooseOrRunOne
              iid
              [ targetLabel
                iid'
                [ FlipClues (InvestigatorTarget iid') 1
                , RemoveDoom (toAbilitySource attrs 1) (InvestigatorTarget iid') 1
                , PlaceDoom (toAbilitySource attrs 1) (LocationTarget lid) 1
                , EnemyDamage yig $ nonAttack attrs 3
                ]
              | iid' <- iids
              ]
        pure a
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      a <$ push (AdvanceAct (toId a) (InvestigatorSource iid) AdvancedWithOther)
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      push $ scenarioResolution 1
      pure a
    _ -> MomentOfDoom <$> runMessage msg attrs
