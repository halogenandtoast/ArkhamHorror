module Arkham.Asset.Assets.FluxStabilizerActive (fluxStabilizerActive) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.SkillTest (getSkillTestId)
import Arkham.Effect.Builder
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Token

newtype FluxStabilizerActive = FluxStabilizerActive AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fluxStabilizerActive :: AssetCard FluxStabilizerActive
fluxStabilizerActive = asset FluxStabilizerActive Cards.fluxStabilizerActive

instance HasAbilities FluxStabilizerActive where
  getAbilities (FluxStabilizerActive a) =
    [ controlled_ a 1
        $ freeReaction
        $ PlacedToken #after AnySource (AssetTargetMatches $ AssetControlledBy You) Clue
    ]

instance RunMessage FluxStabilizerActive where
  runMessage msg a@(FluxStabilizerActive attrs) = runQueueT $ case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      push $ ReplaceInvestigatorAsset iid attrs.id (flipCard $ toCard attrs)
      pure $ FluxStabilizerActive $ attrs & flippedL .~ True
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      getSkillTestId >>= \case
        Just st -> skillTestModifier st attrs iid (AnySkillValue 2)
        Nothing ->
          withSource (attrs.ability 1) $ effect iid do
            apply $ AnySkillValue 2
            during $ #nextSkillTest iid
            removeOn #endOfCurrentPhase
      pure a
    _ -> FluxStabilizerActive <$> liftRunMessage msg attrs
