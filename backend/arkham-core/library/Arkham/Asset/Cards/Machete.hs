module Arkham.Asset.Cards.Machete (
  Machete (..),
  machete,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype Machete = Machete AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

machete :: AssetCard Machete
machete = asset Machete Cards.machete

instance HasModifiersFor Machete where
  getModifiersFor (InvestigatorTarget iid) (Machete attrs) = do
    mSource <- getSkillTestSource
    mTarget <- getSkillTestTarget
    mInvestigator <- getSkillTestInvestigator
    case (mSource, mTarget, mInvestigator) of
      (Just source, Just (EnemyTarget eid), Just iid') | isSource attrs source && iid == iid' -> do
        engagedEnemies <- selectList $ EnemyIsEngagedWith $ InvestigatorWithId iid
        pure $ toModifiers attrs [DamageDealt 1 | engagedEnemies == [eid]]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities Machete where
  getAbilities (Machete a) = [fightAbility a 1 (ActionCost 1) ControlsThis]

instance RunMessage Machete where
  runMessage msg a@(Machete attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll
        [ skillTestModifier attrs iid (SkillModifier SkillCombat 1)
        , ChooseFightEnemy iid source Nothing SkillCombat mempty False
        ]
      pure a
    _ -> Machete <$> runMessage msg attrs
