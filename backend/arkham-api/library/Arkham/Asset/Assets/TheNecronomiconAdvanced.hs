module Arkham.Asset.Assets.TheNecronomiconAdvanced (theNecronomiconAdvanced) where

import Arkham.Ability hiding (you)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (moveTokens)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Script
import GHC.Records

newtype TheNecronomiconAdvanced = TheNecronomiconAdvanced AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Sourceable)

instance HasField "horror" TheNecronomiconAdvanced Int where
  getField = getField @"horror" . toAttrs

theNecronomiconAdvanced :: AssetCard TheNecronomiconAdvanced
theNecronomiconAdvanced =
  assetWith TheNecronomiconAdvanced Cards.theNecronomiconAdvanced
    $ (tokensL %~ setTokens #horror 3)
    . (canLeavePlayByNormalMeansL .~ False)

instance HasModifiersFor TheNecronomiconAdvanced where
  getModifiersFor (TheNecronomiconAdvanced a) = for_ a.controller \iid -> do
    modifySelect
      a
      (ChaosTokenRevealedBy $ be iid)
      [ForcedChaosTokenChange #eldersign [#cultist, #tablet, #elderthing]]

instance HasAbilities TheNecronomiconAdvanced where
  getAbilities (TheNecronomiconAdvanced a) = [controlled a 1 AnyHorrorOnThis actionAbility]

instance RunMessage TheNecronomiconAdvanced where
  runMessage = script do
    revelation placeInYourThreatArea
    onAbilityThen 1
      (moveTokens this you #horror 1)
      (when (this.horror == 0) discardThis)
