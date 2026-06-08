module Arkham.Asset.Assets.BlessingOfIsis3 (blessingOfIsis3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens, withSkillTest)
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Matcher qualified as Matcher
import Arkham.Modifier

newtype BlessingOfIsis3 = BlessingOfIsis3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blessingOfIsis3 :: AssetCard BlessingOfIsis3
blessingOfIsis3 = asset BlessingOfIsis3 Cards.blessingOfIsis3

instance HasAbilities BlessingOfIsis3 where
  getAbilities (BlessingOfIsis3 x) =
    [ wantsSkillTest SkillTestAtYourLocation
        $ controlled x 1 (DuringSkillTest $ SkillTestAtYourLocation <> SkillTestWithRevealedChaosToken #bless)
        $ triggered (Matcher.RevealChaosToken #cancel Anyone #bless) (exhaust x)
    ]

instance RunMessage BlessingOfIsis3 where
  runMessage msg a@(BlessingOfIsis3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> drawnToken) _ -> do
      withSkillTest \sid -> do
        push $ ChaosTokenCanceled iid (attrs.ability 1) drawnToken
        tokens <- nub . (drawnToken :) . filter ((== #bless) . (.face)) <$> getSkillTestRevealedChaosTokens
        for_ tokens \token -> do
          skillTestModifiers sid (attrs.ability 1) token $ ReturnBlessedToChaosBag
            : [ChaosTokenFaceModifier [#eldersign] | token == drawnToken]
        -- Cancelling drops the token from the chaos bag's pending request, so it
        -- would never reach the skill test. Re-reveal it so it resolves with its
        -- modified [elder_sign] face (see parallel Wendy for this pattern).
        push $ RevealChaosToken (SkillTestSource sid) iid drawnToken
        push $ RevealSkillTestChaosTokensAgain iid
      pure a
    _ -> BlessingOfIsis3 <$> liftRunMessage msg attrs
