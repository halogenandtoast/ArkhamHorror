module Arkham.Asset.Cards.OldHuntingRifle3 (oldHuntingRifle3, OldHuntingRifle3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Fight
import Arkham.Message qualified as Msg
import Arkham.Prelude

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
oldHuntingRifle3 = asset (OldHuntingRifle3 . (`with` Metadata NotJammed)) Cards.oldHuntingRifle3

instance HasAbilities OldHuntingRifle3 where
  getAbilities (OldHuntingRifle3 (a `With` Metadata rifleStatus)) =
    fightAbility a 1 (assetUseCost a Ammo 1) (ControlsThis <> jammedRestriction)
      : [ withTooltip "You clear the jam." $ restrictedAbility a 2 ControlsThis actionAbility
        | rifleStatus == Jammed
        ]
   where
    jammedRestriction = case rifleStatus of
      Jammed -> Never
      NotJammed -> NoRestriction

instance RunMessage OldHuntingRifle3 where
  runMessage msg a@(OldHuntingRifle3 (attrs `With` meta)) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      pushAll [skillTestModifiers sid source iid [SkillModifier #combat 3, DamageDealt 2], chooseFight]
      pure a
    Msg.RevealChaosToken (SkillTestSource _) _ t | t.face `elem` [Skull, AutoFail] -> do
      mSkillTestSource <- getSkillTestSource
      for_ mSkillTestSource $ \skillTestSource ->
        pushWhen (isAbilitySource attrs 1 skillTestSource) FailSkillTest
      pure . OldHuntingRifle3 $ attrs `with` Metadata Jammed
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      pure . OldHuntingRifle3 $ attrs `with` Metadata NotJammed
    _ -> OldHuntingRifle3 . (`with` meta) <$> runMessage msg attrs
