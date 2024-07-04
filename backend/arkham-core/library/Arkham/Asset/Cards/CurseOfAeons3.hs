module Arkham.Asset.Cards.CurseOfAeons3 (curseOfAeons3, CurseOfAeons3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens)
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Matcher
import Arkham.Modifier

newtype CurseOfAeons3 = CurseOfAeons3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curseOfAeons3 :: AssetCard CurseOfAeons3
curseOfAeons3 = asset CurseOfAeons3 Cards.curseOfAeons3

instance HasAbilities CurseOfAeons3 where
  getAbilities (CurseOfAeons3 attrs) =
    [ controlledAbility
        attrs
        1
        (DuringSkillTest $ SkillTestAtYourLocation <> SkillTestWithRevealedChaosToken #curse)
        $ ReactionAbility (RevealChaosToken #when Anyone #curse) (exhaust attrs)
    ]

instance RunMessage CurseOfAeons3 where
  runMessage msg a@(CurseOfAeons3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> drawnToken) _ -> do
      tokens <- nub . (drawnToken :) . filter ((== #curse) . (.face)) <$> getSkillTestRevealedChaosTokens

      push $ ChaosTokenCanceled iid (attrs.ability 1) drawnToken
      for_ tokens \token ->
        skillTestModifiers (attrs.ability 1) (ChaosTokenTarget token) $ MayChooseToRemoveChaosToken iid
          : [ChaosTokenFaceModifier [#skull] | token == drawnToken]
      pure a
    _ -> CurseOfAeons3 <$> liftRunMessage msg attrs
