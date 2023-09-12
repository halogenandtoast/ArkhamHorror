module Arkham.Asset.Cards.Pantalone (
  pantalone,
  Pantalone (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillTest
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype Pantalone = Pantalone AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pantalone :: AssetCard Pantalone
pantalone = asset Pantalone Cards.pantalone

instance HasAbilities Pantalone where
  getAbilities (Pantalone a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (AssetEntersPlay Timing.After $ AssetWithId $ toId a)
          Free
    , restrictedAbility a 2 ControlsThis
        $ ReactionAbility
          (InitiatedSkillTest Timing.When You (NotSkillType SkillIntellect) AnySkillTestValue)
          (DiscardCost FromPlay $ toTarget a)
    ]

instance RunMessage Pantalone where
  runMessage msg a@(Pantalone attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushM $ drawCards iid (toAbilitySource attrs 1) 2
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      replaceMessageMatching
        ( \case
            BeginSkillTestAfterFast {} -> True
            Ask _ (ChooseOne (SkillLabel _ (BeginSkillTestAfterFast {} : _) : _)) ->
              True
            _ -> False
        )
        ( \case
            BeginSkillTestAfterFast skillTest ->
              [BeginSkillTest $ skillTest {skillTestType = SkillSkillTest SkillIntellect}]
            Ask _ (ChooseOne (SkillLabel _ (BeginSkillTestAfterFast skillTest : _) : _)) ->
              [BeginSkillTest $ skillTest {skillTestType = SkillSkillTest SkillIntellect}]
            _ -> error "invalid match"
        )
      pure a
    _ -> Pantalone <$> runMessage msg attrs
