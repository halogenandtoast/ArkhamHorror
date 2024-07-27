module Arkham.Asset.Cards.FluxStabilizerActive (
  fluxStabilizerActive,
  FluxStabilizerActive (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Effect.Window
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Token

newtype FluxStabilizerActive = FluxStabilizerActive AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fluxStabilizerActive :: AssetCard FluxStabilizerActive
fluxStabilizerActive = asset FluxStabilizerActive Cards.fluxStabilizerActive

instance HasAbilities FluxStabilizerActive where
  getAbilities (FluxStabilizerActive a) =
    [ restrictedAbility a 1 ControlsThis
        $ freeReaction
        $ PlacedToken #after AnySource (AssetTargetMatches $ AssetControlledBy You) Clue
    ]

instance RunMessage FluxStabilizerActive where
  runMessage msg a@(FluxStabilizerActive attrs) = runQueueT $ case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      push $ ReplaceInvestigatorAsset iid attrs.id (flipCard $ toCard attrs)
      pure $ FluxStabilizerActive $ attrs & flippedL .~ True
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push
        $ CreateWindowModifierEffect
          (FirstEffectWindow [EffectNextSkillTestWindow, EffectPhaseWindow])
          (effectModifiers (attrs.ability 1) [AnySkillValue 2])
          (attrs.ability 1)
          (toTarget iid)
      pure a
    _ -> FluxStabilizerActive <$> liftRunMessage msg attrs
