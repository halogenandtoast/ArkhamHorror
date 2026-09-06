module Arkham.Asset.Assets.ChosenOfZburamoarteFightingTheHunger (
  chosenOfZburamoarteFightingTheHunger,
) where

import Arkham.Ability
import Arkham.Asset.Cards.ChildrenOfBlood qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosToken
import Arkham.Helpers.ChaosBag (getRemainingBloodTokens)
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFace)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (withSkillTestSource)
import Arkham.Matcher hiding (RevealChaosToken)

newtype ChosenOfZburamoarteFightingTheHunger = ChosenOfZburamoarteFightingTheHunger AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chosenOfZburamoarteFightingTheHunger :: AssetCard ChosenOfZburamoarteFightingTheHunger
chosenOfZburamoarteFightingTheHunger =
  asset ChosenOfZburamoarteFightingTheHunger Cards.chosenOfZburamoarteFightingTheHunger

instance HasModifiersFor ChosenOfZburamoarteFightingTheHunger where
  getModifiersFor (ChosenOfZburamoarteFightingTheHunger a) =
    modifyEach a [toTarget BloodToken] [RevealAnotherChaosToken]

instance HasAbilities ChosenOfZburamoarteFightingTheHunger where
  getAbilities (ChosenOfZburamoarteFightingTheHunger a) =
    [controlled a 1 ControlsThis $ forced $ GameBegins #when]

instance RunMessage ChosenOfZburamoarteFightingTheHunger where
  runMessage msg a@(ChosenOfZburamoarteFightingTheHunger attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- min 5 <$> getRemainingBloodTokens
      repeated n $ addChaosTokenForGame #blood
      pure a
    RevealChaosToken _ iid token -> do
      withSkillTestSource \_ -> do
        faces <- getModifiedChaosTokenFace token
        when (BloodToken `elem` faces) $ afterSkillTestQuiet do
          push $ Flip iid (toSource attrs) (toTarget attrs)
          sealChaosToken iid attrs token
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.chosenOfZburamoarteCompelledToFeed
      pure a
    _ -> ChosenOfZburamoarteFightingTheHunger <$> liftRunMessage msg attrs
