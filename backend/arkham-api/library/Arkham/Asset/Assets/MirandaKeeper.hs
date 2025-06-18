module Arkham.Asset.Assets.MirandaKeeper (mirandaKeeper) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Window
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), effectModifiers)
import Arkham.Matcher
import Arkham.Token qualified as Token

newtype MirandaKeeper = MirandaKeeper AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mirandaKeeper :: AssetCard MirandaKeeper
mirandaKeeper = allyWith MirandaKeeper Cards.mirandaKeeper (2, 2) noSlots

instance HasAbilities MirandaKeeper where
  getAbilities (MirandaKeeper a) =
    [ restricted a 1 ControlsThis $ FastAbility (assetUseCost a Token.Supply 1)
    , controlled a 2 (TokensOnLocation YourLocation Token.Antiquity (atLeast 1))
        $ freeReaction (SkillTestResult #after You AnySkillTest (SuccessResult $ atLeast 2))
    ]

instance RunMessage MirandaKeeper where
  runMessage msg a@(MirandaKeeper attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> placeTokens (attrs.ability 1) lid Token.Antiquity 1
      ems <- effectModifiers (attrs.ability 1) [AnySkillValue 2]
      push
        $ CreateWindowModifierEffect
          (FirstEffectWindow [EffectNextSkillTestWindow iid, EffectRoundWindow])
          ems
          (attrs.ability 1)
          (toTarget iid)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withLocationOf iid \lid -> removeTokens (attrs.ability 2) lid Token.Antiquity 1
      gainResources iid (attrs.ability 2) 2
      pure a
    Flip _ ScenarioSource (isTarget attrs -> True) -> do
      pure $ MirandaKeeper $ attrs & flippedL .~ True & visibleL .~ False
    Flip _ _ (isTarget attrs -> True) -> do
      let flipped = not $ view flippedL attrs
      pure $ MirandaKeeper $ attrs & flippedL .~ flipped & visibleL .~ True
    _ -> MirandaKeeper <$> liftRunMessage msg attrs
