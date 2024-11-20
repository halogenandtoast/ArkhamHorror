module Arkham.Asset.Assets.BlessingOfIsis3 (blessingOfIsis3, BlessingOfIsis3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens, withSkillTest)
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Matcher
import Arkham.Modifier

newtype BlessingOfIsis3 = BlessingOfIsis3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blessingOfIsis3 :: AssetCard BlessingOfIsis3
blessingOfIsis3 = asset BlessingOfIsis3 Cards.blessingOfIsis3

instance HasAbilities BlessingOfIsis3 where
  getAbilities (BlessingOfIsis3 x) =
    [ wantsSkillTest SkillTestAtYourLocation
        $ controlled
          x
          1
          (DuringSkillTest $ SkillTestAtYourLocation <> SkillTestWithRevealedChaosToken #bless)
        $ triggered (RevealChaosToken #when Anyone #bless) (exhaust x)
    ]

instance RunMessage BlessingOfIsis3 where
  runMessage msg a@(BlessingOfIsis3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> drawnToken) _ -> do
      withSkillTest \sid -> do
        push $ ChaosTokenCanceled iid (attrs.ability 1) drawnToken
        tokens <- nub . (drawnToken :) . filter ((== #bless) . (.face)) <$> getSkillTestRevealedChaosTokens
        for_ tokens \token -> do
          skillTestModifiers sid (attrs.ability 1) (ChaosTokenTarget token) $ ReturnBlessedToChaosBag
            : [ChaosTokenFaceModifier [#eldersign] | token == drawnToken]
      pure a
    _ -> BlessingOfIsis3 <$> liftRunMessage msg attrs
