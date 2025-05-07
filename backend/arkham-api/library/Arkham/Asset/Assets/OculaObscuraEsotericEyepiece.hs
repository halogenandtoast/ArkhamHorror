module Arkham.Asset.Assets.OculaObscuraEsotericEyepiece (oculaObscuraEsotericEyepiece) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosBagStepState
import Arkham.ChaosToken.Types
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens, getSkillTestId)
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Message.Lifted.Choose

newtype OculaObscuraEsotericEyepiece = OculaObscuraEsotericEyepiece AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oculaObscuraEsotericEyepiece :: AssetCard OculaObscuraEsotericEyepiece
oculaObscuraEsotericEyepiece = asset OculaObscuraEsotericEyepiece Cards.oculaObscuraEsotericEyepiece

instance HasAbilities OculaObscuraEsotericEyepiece where
  getAbilities (OculaObscuraEsotericEyepiece x) =
    [ playerLimit PerPhase
        $ controlled x 1 (criteria1 <> DuringSkillTest (SkillTestWithRevealedChaosToken $ notSealedToken NonSymbol))
        $ freeReaction (SkillTestResult #after You #any #success)
    , playerLimit PerTestOrAbility $ controlled x 2 criteria2 $ forced $ WouldRevealChaosToken #when You
    ]
   where
    criteria1 = if null x.sealedChaosTokens then NoRestriction else Never
    criteria2 = if null x.sealedChaosTokens then Never else NoRestriction
    notSealedToken matcher = case x.sealedChaosTokens of
      [] -> matcher
      xs -> matcher <> not_ (mapOneOf (ChaosTokenIs . chaosTokenId) xs)

instance RunMessage OculaObscuraEsotericEyepiece where
  runMessage msg a@(OculaObscuraEsotericEyepiece attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      xs <- filter (\c -> not $ isSymbolChaosToken c.face) <$> getSkillTestRevealedChaosTokens
      focusChaosTokens xs \unfocus -> do
        chooseTargetM iid xs (sealChaosToken iid attrs)
        push unfocus
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let tokens = map (\t -> t { chaosTokenSealed = True }) attrs.sealedChaosTokens
      push
        $ ReplaceCurrentDraw (attrs.ability 2) iid
        $ Choose (attrs.ability 2) 1 ResolveChoice [Resolved tokens] [] Nothing
      getSkillTestId >>= \case
        Nothing -> for_ attrs.sealedChaosTokens unsealChaosToken
        Just _ -> afterSkillTestQuiet $ for_ attrs.sealedChaosTokens unsealChaosToken
      pure a
    _ -> OculaObscuraEsotericEyepiece <$> liftRunMessage msg attrs
