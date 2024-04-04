module Arkham.Asset.Cards.BlessingOfIsis3 (blessingOfIsis3, BlessingOfIsis3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype BlessingOfIsis3 = BlessingOfIsis3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blessingOfIsis3 :: AssetCard BlessingOfIsis3
blessingOfIsis3 = asset BlessingOfIsis3 Cards.blessingOfIsis3

instance HasAbilities BlessingOfIsis3 where
  getAbilities (BlessingOfIsis3 x) =
    [ controlledAbility
        x
        1
        (DuringSkillTest $ SkillTestAtYourLocation <> SkillTestWithRevealedChaosToken #bless)
        $ ReactionAbility (RevealChaosToken #when Anyone #bless) (exhaust x)
    ]

instance RunMessage BlessingOfIsis3 where
  runMessage msg a@(BlessingOfIsis3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> drawnToken) _ -> do
      tokens <- nub . (drawnToken :) . filter ((== #bless) . (.face)) <$> getSkillTestRevealedChaosTokens

      pushAll $ ChaosTokenCanceled iid (attrs.ability 1) drawnToken
        : [ skillTestModifiers (attrs.ability 1) (ChaosTokenTarget token) $ ReturnBlessedToChaosBag
            : [ChaosTokenFaceModifier [#eldersign] | token == drawnToken]
          | token <- tokens
          ]
      pure a
    _ -> BlessingOfIsis3 <$> runMessage msg attrs
