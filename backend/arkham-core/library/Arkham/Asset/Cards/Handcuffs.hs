module Arkham.Asset.Cards.Handcuffs
  ( handcuffs
  , Handcuffs(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher hiding ( EnemyEvaded )
import Arkham.Placement
import Arkham.SkillType
import Arkham.Trait ( Trait (Humanoid) )

newtype Handcuffs = Handcuffs AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

handcuffs :: AssetCard Handcuffs
handcuffs = asset Handcuffs Cards.handcuffs

instance HasModifiersFor Handcuffs where
  getModifiersFor (EnemyTarget eid) (Handcuffs a) | attachedToEnemy a eid = do
    isNonElite <- eid <=~> NonEliteEnemy
    pure $ toModifiers a $ do
      guard isNonElite
      [CannotReady, CannotPlaceDoomOnThis]
  getModifiersFor _ _ = pure []

instance HasAbilities Handcuffs where
  getAbilities (Handcuffs a) = case assetPlacement a of
    AttachedToEnemy _ -> []
    _ ->
      [ restrictedAbility
            a
            1
            (ControlsThis <> EnemyCriteria
              (EnemyExists $ CanEvadeEnemy (toSource a) <> EnemyWithTrait Humanoid)
            )
          $ ActionAbility (Just Action.Evade)
          $ ActionCost 1
      ]

instance RunMessage Handcuffs where
  runMessage msg a@(Handcuffs attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ ChooseEvadeEnemy
        iid
        (toSource attrs)
        (Just $ toTarget attrs)
        SkillCombat
        (EnemyWithTrait Humanoid)
        False
      pure a
    Successful (Action.Evade, EnemyTarget enemyId) iid _ (isTarget attrs -> True) _
      -> do
        pushAll
          [ EnemyEvaded iid enemyId
          , PlaceAsset (toId attrs) (AttachedToEnemy enemyId)
          ]
        pure a
    _ -> Handcuffs <$> runMessage msg attrs
