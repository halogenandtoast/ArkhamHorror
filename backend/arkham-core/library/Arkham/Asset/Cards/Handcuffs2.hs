module Arkham.Asset.Cards.Handcuffs2 (
  handcuffs2,
  Handcuffs2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Placement
import Arkham.SkillType
import Arkham.Trait (Trait (Humanoid))

newtype Handcuffs2 = Handcuffs2 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

handcuffs2 :: AssetCard Handcuffs2
handcuffs2 = asset Handcuffs2 Cards.handcuffs2

instance HasModifiersFor Handcuffs2 where
  getModifiersFor (EnemyTarget eid) (Handcuffs2 a) | attachedToEnemy a eid = do
    isNonElite <- eid <=~> NonEliteEnemy
    pure $ toModifiers a $ do
      guard isNonElite
      [CannotReady, CannotPlaceDoomOnThis]
  getModifiersFor _ _ = pure []

instance HasAbilities Handcuffs2 where
  getAbilities (Handcuffs2 a) = case assetPlacement a of
    AttachedToEnemy _ -> []
    _ ->
      [ restrictedAbility
          a
          1
          ( ControlsThis
              <> EnemyCriteria
                (EnemyExists $ CanEvadeEnemy (toSource a) <> EnemyWithTrait Humanoid)
          )
          $ ActionAbility [Action.Evade]
          $ ActionCost 1
      ]

instance RunMessage Handcuffs2 where
  runMessage msg a@(Handcuffs2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push
        $ ChooseEvadeEnemy
          iid
          (toSource attrs)
          (Just $ toTarget attrs)
          SkillCombat
          (EnemyWithTrait Humanoid)
          False
      pure a
    Successful (Action.Evade, EnemyTarget enemyId) iid _ (isTarget attrs -> True) _ ->
      do
        pushAll
          [ EnemyEvaded iid enemyId
          , RemoveAllDoom (toAbilitySource attrs 1) (toTarget enemyId)
          , PlaceAsset (toId attrs) (AttachedToEnemy enemyId)
          ]
        pure a
    _ -> Handcuffs2 <$> runMessage msg attrs
