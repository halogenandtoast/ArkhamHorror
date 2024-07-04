module Arkham.Asset.Cards.SacredCovenant2 (sacredCovenant2, SacredCovenant2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Matcher
import Arkham.Prelude
import Arkham.SkillTest.Base
import Arkham.SkillTest.Step

newtype SacredCovenant2 = SacredCovenant2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sacredCovenant2 :: AssetCard SacredCovenant2
sacredCovenant2 = asset SacredCovenant2 Cards.sacredCovenant2

instance HasAbilities SacredCovenant2 where
  getAbilities (SacredCovenant2 attrs) =
    [ restrictedAbility attrs 1 (ControlsThis <> DuringSkillTest (SkillTestWithRevealedChaosToken #bless))
        $ ReactionAbility (SkillTestStep #after RevealChaosTokenStep) (exhaust attrs)
    ]

instance RunMessage SacredCovenant2 where
  runMessage msg a@(SacredCovenant2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      st <- fromJustNote "must be during skill test" <$> getSkillTest
      let blessTokens = filter ((== #bless) . chaosTokenFace) (skillTestRevealedChaosTokens st)
      player <- getPlayer iid
      push
        $ chooseUpToN
          player
          (length blessTokens)
          "Done returning bless tokens"
          [targetLabel (ChaosTokenTarget token) [ReturnChaosTokens [token]] | token <- blessTokens]
      pure a
    _ -> SacredCovenant2 <$> runMessage msg attrs
