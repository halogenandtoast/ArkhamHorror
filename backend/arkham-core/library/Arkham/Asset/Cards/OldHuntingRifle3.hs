module Arkham.Asset.Cards.OldHuntingRifle3
  ( oldHuntingRifle3
  , OldHuntingRifle3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target
import Arkham.Token

data RifleStatus = NotJammed | Jammed
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Metadata = Metadata { rifleStatus :: RifleStatus }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype OldHuntingRifle3 = OldHuntingRifle3 (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldHuntingRifle3 :: AssetCard OldHuntingRifle3
oldHuntingRifle3 =
  asset (OldHuntingRifle3 . (`with` Metadata NotJammed)) Cards.oldHuntingRifle3

instance HasAbilities OldHuntingRifle3 where
  getAbilities (OldHuntingRifle3 (a `With` Metadata rifleStatus)) =
    restrictedAbility
        a
        1
        (ControlsThis <> jammedRestriction)
        (ActionAbility (Just Action.Fight) $ ActionCost 1 <> UseCost
          (AssetWithId $ toId a)
          Ammo
          1
        )
      : [ withTooltip "You clear the jam."
          $ restrictedAbility a 2 ControlsThis
          $ ActionAbility Nothing
          $ ActionCost 1
        | rifleStatus == Jammed
        ]
   where
    jammedRestriction = case rifleStatus of
      Jammed -> Never
      NotJammed -> NoRestriction

instance RunMessage OldHuntingRifle3 where
  runMessage msg a@(OldHuntingRifle3 (attrs `With` meta@(Metadata rifleStatus)))
    = case msg of
      UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
        pushAll
          [ skillTestModifiers
            (toSource attrs)
            (InvestigatorTarget iid)
            [SkillModifier SkillCombat 3, DamageDealt 2]
          , ChooseFightEnemy
            iid
            (toSource attrs)
            Nothing
            SkillCombat
            AnyEnemy
            False
          ]
        pure a
      RevealToken (isSkillTestSource attrs -> True) _ t
        | tokenFace t `elem` [Skull, AutoFail] -> do
          push FailSkillTest
          pure . OldHuntingRifle3 $ attrs `with` Metadata Jammed
      UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
        pure . OldHuntingRifle3 $ attrs `with` Metadata NotJammed
      _ -> OldHuntingRifle3 . (`with` meta) <$> runMessage msg attrs
