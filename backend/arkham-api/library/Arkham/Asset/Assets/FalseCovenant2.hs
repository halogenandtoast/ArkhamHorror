module Arkham.Asset.Assets.FalseCovenant2 (falseCovenant2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Helpers.ChaosToken ()
import Arkham.Helpers.SkillTest
import Arkham.Helpers.Window
import Arkham.Matcher

newtype FalseCovenant2 = FalseCovenant2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falseCovenant2 :: AssetCard FalseCovenant2
falseCovenant2 = asset FalseCovenant2 Cards.falseCovenant2

instance HasAbilities FalseCovenant2 where
  getAbilities (FalseCovenant2 a) =
    [ controlled a 1 (DuringSkillTest AnySkillTest)
        $ triggered
          (RevealChaosToken #when (affectsOthers $ colocatedWithMatch You) #curse)
          (exhaust a)
    ]

instance RunMessage FalseCovenant2 where
  runMessage msg a@(FalseCovenant2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      iid' <- fromJustNote "missing investigator" <$> getSkillTestInvestigator
      cancelChaosToken (attrs.ability 1) iid token
      push $ ReturnChaosTokensToPool [token]
      unfocusChaosTokens
      drawAnotherChaosToken iid'
      pure a
    _ -> FalseCovenant2 <$> liftRunMessage msg attrs
