module Arkham.Asset.Cards.Machete (
  Machete (..),
  machete,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
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
    mSkillTestSource <- getSkillTestSource
    mSkillTestTarget <- getSkillTestTarget
    case (mSkillTestTarget, mSkillTestSource) of
      (Just (EnemyTarget eid), Just (SkillTestSource iid' _ source _))
        | isSource attrs source && iid == iid' -> do
            engagedEnemies <-
              selectList $
                EnemyIsEngagedWith $
                  InvestigatorWithId
                    iid
            pure $ toModifiers attrs [DamageDealt 1 | engagedEnemies == [eid]]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities Machete where
  getAbilities (Machete a) =
    [ restrictedAbility a 1 ControlsThis $
        ActionAbility (Just Action.Fight) $
          ActionCost 1
    ]

instance RunMessage Machete where
  runMessage msg a@(Machete attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll
        [ skillTestModifier
            attrs
            (InvestigatorTarget iid)
            (SkillModifier SkillCombat 1)
        , ChooseFightEnemy iid source Nothing SkillCombat mempty False
        ]
      pure a
    _ -> Machete <$> runMessage msg attrs
