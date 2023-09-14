module Arkham.Asset.Cards.Machete (
  Machete (..),
  machete,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Control.Monad.Trans.Maybe

newtype Machete = Machete AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

machete :: AssetCard Machete
machete = asset Machete Cards.machete

instance HasModifiersFor Machete where
  getModifiersFor (InvestigatorTarget iid) (Machete attrs) = do
    mods <- runMaybeT $ do
      (isAbilitySource attrs 1 -> True) <- MaybeT getSkillTestSource
      EnemyTarget eid <- MaybeT getSkillTestTarget
      engagedEnemies <- lift $ selectList $ enemyEngagedWith iid
      guard $ engagedEnemies == [eid]
      iid' <- MaybeT getSkillTestInvestigator
      guard $ iid == iid'
      pure $ DamageDealt 1
    pure $ toModifiers attrs $ maybeToList mods
  getModifiersFor _ _ = pure []

instance HasAbilities Machete where
  getAbilities (Machete a) = [fightAbility a 1 (ActionCost 1) ControlsThis]

instance RunMessage Machete where
  runMessage msg a@(Machete attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ skillTestModifier (toAbilitySource attrs 1) iid (SkillModifier #combat 1)
        , chooseFightEnemy iid (toAbilitySource attrs 1) #combat
        ]
      pure a
    _ -> Machete <$> runMessage msg attrs
