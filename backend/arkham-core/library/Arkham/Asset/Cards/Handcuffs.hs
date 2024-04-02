module Arkham.Asset.Cards.Handcuffs (handcuffs, Handcuffs (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Evade
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Placement
import Arkham.Prelude
import Arkham.Trait (Trait (Humanoid))

newtype Handcuffs = Handcuffs AssetAttrs
  deriving anyclass (IsAsset)
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
      [ controlledAbility a 1 (exists $ CanEvadeEnemy (toSource a) <> EnemyWithTrait Humanoid) evadeAction_
      ]

instance RunMessage Handcuffs where
  runMessage msg a@(Handcuffs attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseEvade <-
        leftOr
          <$> aspect
            iid
            source
            (#combat `InsteadOf` #agility)
            (setTarget attrs <$> mkChooseEvadeMatch iid source (EnemyWithTrait Humanoid))
      pushAll chooseEvade
      pure a
    Successful (Action.Evade, EnemyTarget enemyId) iid _ (isTarget attrs -> True) _ ->
      do
        pushAll
          [ EnemyEvaded iid enemyId
          , PlaceAsset (toId attrs) (AttachedToEnemy enemyId)
          ]
        pure a
    _ -> Handcuffs <$> runMessage msg attrs
