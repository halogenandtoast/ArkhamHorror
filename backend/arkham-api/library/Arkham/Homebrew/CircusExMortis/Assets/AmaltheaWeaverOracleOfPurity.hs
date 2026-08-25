module Arkham.Homebrew.CircusExMortis.Assets.AmaltheaWeaverOracleOfPurity (
  amaltheaWeaverOracleOfPurity,
) where

import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.DestinyAndProphecy

newtype AmaltheaWeaverOracleOfPurity = AmaltheaWeaverOracleOfPurity AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

amaltheaWeaverOracleOfPurity :: AssetCard AmaltheaWeaverOracleOfPurity
amaltheaWeaverOracleOfPurity =
  ally AmaltheaWeaverOracleOfPurity Cards.amaltheaWeaverOracleOfPurity (4, 2)

instance HasModifiersFor AmaltheaWeaverOracleOfPurity where
  getModifiersFor (AmaltheaWeaverOracleOfPurity a) =
    controllerGets a [SkillModifier #willpower 1]

instance HasAbilities AmaltheaWeaverOracleOfPurity where
  getAbilities (AmaltheaWeaverOracleOfPurity a) = amaltheaWeaverAbilities a

instance RunMessage AmaltheaWeaverOracleOfPurity where
  runMessage msg a@(AmaltheaWeaverOracleOfPurity attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      amaltheaWeaverBoost attrs
      amaltheaWeaverRider attrs msg
      pure a
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      amaltheaWeaverRelease attrs 2
      pure a
    _ -> AmaltheaWeaverOracleOfPurity <$> liftRunMessage msg attrs
