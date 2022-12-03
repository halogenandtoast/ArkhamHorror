module Arkham.Asset.Cards.Pantalone
  ( pantalone
  , Pantalone(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
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
    , restrictedAbility a 2 ControlsThis $ ReactionAbility
      (InitiatedSkillTest Timing.When You (NotSkillType SkillIntellect) AnySkillTestValue)
      (DiscardCost $ toTarget a)
    ]

instance RunMessage Pantalone where
  runMessage msg a@(Pantalone attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ drawCards iid attrs 2
      pure a
    UseCardAbility _ source 2 _ _
      | isSource attrs source
      -> do
        replaceMessageMatching
          (\case
            BeginSkillTestAfterFast{} -> True
            Ask _ (ChooseOne (SkillLabel _ (BeginSkillTestAfterFast{} : _) : _))
              -> True
            _ -> False
          )
          (\case
            BeginSkillTestAfterFast iid' source' target' maction' _ difficulty'
              -> [ BeginSkillTest
                     iid'
                     source'
                     target'
                     maction'
                     SkillIntellect
                     difficulty'
                 ]
            Ask _ (ChooseOne (SkillLabel _ (BeginSkillTestAfterFast iid' source' target' maction' _ difficulty' : _) : _))
              -> [ BeginSkillTest
                     iid'
                     source'
                     target'
                     maction'
                     SkillIntellect
                     difficulty'
                 ]
            _ -> error "invalid match"
          )
        pure a
    _ -> Pantalone <$> runMessage msg attrs
