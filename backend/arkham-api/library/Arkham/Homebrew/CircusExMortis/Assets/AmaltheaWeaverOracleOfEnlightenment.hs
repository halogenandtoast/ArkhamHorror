module Arkham.Homebrew.CircusExMortis.Assets.AmaltheaWeaverOracleOfEnlightenment (
  amaltheaWeaverOracleOfEnlightenment,
) where

import Arkham.Asset.Import.Lifted
import Arkham.Draw.Types (CardDrawRules (AfterDrawDiscard), withCardDrawRule)
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.DestinyAndProphecy

newtype AmaltheaWeaverOracleOfEnlightenment = AmaltheaWeaverOracleOfEnlightenment AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

amaltheaWeaverOracleOfEnlightenment :: AssetCard AmaltheaWeaverOracleOfEnlightenment
amaltheaWeaverOracleOfEnlightenment =
  ally AmaltheaWeaverOracleOfEnlightenment Cards.amaltheaWeaverOracleOfEnlightenment (2, 4)

instance HasModifiersFor AmaltheaWeaverOracleOfEnlightenment where
  getModifiersFor (AmaltheaWeaverOracleOfEnlightenment a) =
    controllerGets a [SkillModifier #willpower 1]

instance HasAbilities AmaltheaWeaverOracleOfEnlightenment where
  getAbilities (AmaltheaWeaverOracleOfEnlightenment a) = amaltheaWeaverAbilities a

instance RunMessage AmaltheaWeaverOracleOfEnlightenment where
  runMessage msg a@(AmaltheaWeaverOracleOfEnlightenment attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      amaltheaWeaverBoost attrs
      amaltheaWeaverRider attrs msg
      pure a
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      amaltheaWeaverChooseRecipient attrs \iid ->
        drawCardsEdit iid (attrs.ability 1) 2 (withCardDrawRule (AfterDrawDiscard 1))
      pure a
    _ -> AmaltheaWeaverOracleOfEnlightenment <$> liftRunMessage msg attrs
