module Arkham.Homebrew.CircusExMortis.Assets.AmaltheaWeaverOracleOfResolve (
  amaltheaWeaverOracleOfResolve,
) where

import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.DestinyAndProphecy

newtype AmaltheaWeaverOracleOfResolve = AmaltheaWeaverOracleOfResolve AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

amaltheaWeaverOracleOfResolve :: AssetCard AmaltheaWeaverOracleOfResolve
amaltheaWeaverOracleOfResolve =
  ally AmaltheaWeaverOracleOfResolve Cards.amaltheaWeaverOracleOfResolve (3, 3)

instance HasModifiersFor AmaltheaWeaverOracleOfResolve where
  getModifiersFor (AmaltheaWeaverOracleOfResolve a) =
    controllerGets a [SkillModifier #willpower 1, SkillModifier #combat 1]

instance HasAbilities AmaltheaWeaverOracleOfResolve where
  getAbilities (AmaltheaWeaverOracleOfResolve a) = amaltheaWeaverAbilities a

instance RunMessage AmaltheaWeaverOracleOfResolve where
  runMessage msg a@(AmaltheaWeaverOracleOfResolve attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      amaltheaWeaverBoost attrs
      amaltheaWeaverRider attrs msg
      pure a
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      amaltheaWeaverRelease attrs 1
      pure a
    _ -> AmaltheaWeaverOracleOfResolve <$> liftRunMessage msg attrs
