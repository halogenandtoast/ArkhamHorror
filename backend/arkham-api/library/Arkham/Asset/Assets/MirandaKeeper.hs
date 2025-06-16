module Arkham.Asset.Assets.MirandaKeeper (
  mirandaKeeper,
  MirandaKeeper(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Window
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher

newtype MirandaKeeper = MirandaKeeper AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mirandaKeeper :: AssetCard MirandaKeeper
mirandaKeeper = allyWith MirandaKeeper Cards.mirandaKeeper (2, 2) noSlots

instance HasAbilities MirandaKeeper where
  getAbilities (MirandaKeeper a) =
    [ controlledAbility a 1 ControlsThis $ FastAbility (assetUseCost a Supply 1)
    , restrictedAbility
        a
        2
        (ControlsThis <> TokensOnLocation YouLocation Token.Antiquity (atLeast 1))
        $ ReactionAbility
          (SkillTestResult #after You AnySkillTest (SuccessResult $ atLeast 2))
          Free
    ]

instance RunMessage MirandaKeeper where
  runMessage msg a@(MirandaKeeper attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> placeTokens (attrs.ability 1) lid Token.Antiquity 1
      ems <- effectModifiers (attrs.ability 1) [AnySkillValue 2]
      push $ CreateWindowModifierEffect (FirstEffectWindow [EffectNextSkillTestWindow iid, EffectRoundWindow]) ems (attrs.ability 1) (toTarget iid)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withLocationOf iid \lid -> removeTokens (attrs.ability 2) lid Token.Antiquity 1
      gainResources iid (attrs.ability 2) 2
      pure a
    _ -> MirandaKeeper <$> liftRunMessage msg attrs
