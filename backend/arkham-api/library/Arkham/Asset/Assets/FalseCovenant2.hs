module Arkham.Asset.Assets.FalseCovenant2 (falseCovenant2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.ChaosToken
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Prelude

newtype FalseCovenant2 = FalseCovenant2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falseCovenant2 :: AssetCard FalseCovenant2
falseCovenant2 = asset FalseCovenant2 Cards.falseCovenant2

instance HasAbilities FalseCovenant2 where
  getAbilities (FalseCovenant2 a) =
    [ restricted a 1 ControlsThis
        $ triggered
          (RevealChaosToken #when (affectsOthers $ colocatedWithMatch You) #curse)
          (exhaust a)
    ]

instance RunMessage FalseCovenant2 where
  runMessage msg a@(FalseCovenant2 attrs) = case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      let source = toAbilitySource attrs 1
      iid <- fromJustNote "missing investigator" <$> getSkillTestInvestigator
      cancelChaosToken token
      pushAll
        [ CancelEachNext Nothing source [CheckWindowMessage, DrawChaosTokenMessage, RevealChaosTokenMessage]
        , ReturnChaosTokensToPool [token]
        , UnfocusChaosTokens
        , DrawAnotherChaosToken iid
        ]
      pure a
    _ -> FalseCovenant2 <$> runMessage msg attrs
