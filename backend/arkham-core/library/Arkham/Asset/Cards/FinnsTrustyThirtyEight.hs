module Arkham.Asset.Cards.FinnsTrustyThirtyEight
  ( finnsTrustyThirtyEight
  , FinnsTrustyThirtyEight(..)
  )
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillTest
import Arkham.SkillType
import Arkham.Source
import Arkham.Target

newtype FinnsTrustyThirtyEight = FinnsTrustyThirtyEight AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

finnsTrustyThirtyEight :: AssetCard FinnsTrustyThirtyEight
finnsTrustyThirtyEight =
  asset FinnsTrustyThirtyEight Cards.finnsTrustyThirtyEight

instance HasModifiersFor FinnsTrustyThirtyEight where
  getModifiersFor _ (InvestigatorTarget iid) (FinnsTrustyThirtyEight attrs) = do
    mSkillTestSource <- getSkillTestSource
    mSkillTestTarget <- getSkillTestTarget
    case (mSkillTestTarget, mSkillTestSource) of
      (Just (EnemyTarget eid), Just (SkillTestSource iid' _ source _))
        | isSource attrs source && iid == iid' -> do
          engagedEnemies <- selectList $ EnemyIsEngagedWith $ InvestigatorWithId
            iid
          pure $ toModifiers attrs [ DamageDealt 1 | eid `notElem` engagedEnemies ]
      _ -> pure []
  getModifiersFor _ _ _ = pure []

instance HasAbilities FinnsTrustyThirtyEight where
  getAbilities (FinnsTrustyThirtyEight a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility (Just Action.Fight)
        $ ActionCost 1 <> UseCost (AssetWithId $ toId a) Ammo 1
    ]

instance RunMessage FinnsTrustyThirtyEight where
  runMessage msg a@(FinnsTrustyThirtyEight attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      a <$ pushAll
        [ skillTestModifier
          attrs
          (InvestigatorTarget iid)
          (SkillModifier SkillCombat 2)
        , ChooseFightEnemy iid source Nothing SkillCombat mempty False
        ]
    _ -> FinnsTrustyThirtyEight <$> runMessage msg attrs
