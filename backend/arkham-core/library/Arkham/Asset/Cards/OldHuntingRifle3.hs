module Arkham.Asset.Cards.OldHuntingRifle3 (
  oldHuntingRifle3,
  OldHuntingRifle3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Message qualified as Msg
import Arkham.SkillType

data RifleStatus = NotJammed | Jammed
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Metadata = Metadata {rifleStatus :: RifleStatus}
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
    fightAbility
      a
      1
      (assetUseCost a Ammo 1)
      (ControlsThis <> jammedRestriction)
      : [ withTooltip "You clear the jam."
          $ restrictedAbility a 2 ControlsThis
          $ ActionAbility [] (ActionCost 1)
        | rifleStatus == Jammed
        ]
   where
    jammedRestriction = case rifleStatus of
      Jammed -> Never
      NotJammed -> NoRestriction

instance RunMessage OldHuntingRifle3 where
  runMessage msg a@(OldHuntingRifle3 (attrs `With` meta)) =
    case msg of
      UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
        pushAll
          [ skillTestModifiers attrs iid [SkillModifier SkillCombat 3, DamageDealt 2]
          , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat AnyEnemy False
          ]
        pure a
      Msg.RevealChaosToken SkillTestSource _ t | chaosTokenFace t `elem` [Skull, AutoFail] -> do
        mSkillTestSource <- getSkillTestSource
        for_ mSkillTestSource $ \skillTestSource ->
          pushWhen (isSource attrs skillTestSource) FailSkillTest
        pure . OldHuntingRifle3 $ attrs `with` Metadata Jammed
      UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
        pure . OldHuntingRifle3 $ attrs `with` Metadata NotJammed
      _ -> OldHuntingRifle3 . (`with` meta) <$> runMessage msg attrs
