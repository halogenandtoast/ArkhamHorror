module Arkham.Asset.Cards.FinnsTrustyThirtyEight (
  finnsTrustyThirtyEight,
  FinnsTrustyThirtyEight (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype FinnsTrustyThirtyEight = FinnsTrustyThirtyEight AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

finnsTrustyThirtyEight :: AssetCard FinnsTrustyThirtyEight
finnsTrustyThirtyEight =
  asset FinnsTrustyThirtyEight Cards.finnsTrustyThirtyEight

instance HasModifiersFor FinnsTrustyThirtyEight where
  getModifiersFor (InvestigatorTarget iid) (FinnsTrustyThirtyEight attrs) = do
    mSource <- getSkillTestSource
    mTarget <- getSkillTestTarget
    miid <- getSkillTestInvestigator
    case (mSource, mTarget, miid) of
      (Just source, Just (EnemyTarget eid), Just iid')
        | isSource attrs source && iid == iid' -> do
            engagedEnemies <- selectList $ EnemyIsEngagedWith $ InvestigatorWithId iid
            pure $ toModifiers attrs [DamageDealt 1 | eid `notElem` engagedEnemies]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities FinnsTrustyThirtyEight where
  getAbilities (FinnsTrustyThirtyEight a) =
    [fightAbility a 1 (assetUseCost a Ammo 1) ControlsThis]

instance RunMessage FinnsTrustyThirtyEight where
  runMessage msg a@(FinnsTrustyThirtyEight attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll
        [ skillTestModifier attrs iid (SkillModifier SkillCombat 2)
        , ChooseFightEnemy iid source Nothing SkillCombat mempty False
        ]
      pure a
    _ -> FinnsTrustyThirtyEight <$> runMessage msg attrs
