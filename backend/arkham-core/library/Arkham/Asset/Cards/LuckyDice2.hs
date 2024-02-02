module Arkham.Asset.Cards.LuckyDice2 (
  luckyDice2,
  LuckyDice2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window

newtype LuckyDice2 = LuckyDice2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

luckyDice2 :: AssetCard LuckyDice2
luckyDice2 = asset LuckyDice2 Cards.luckyDice2

instance HasAbilities LuckyDice2 where
  getAbilities (LuckyDice2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (RevealChaosToken Timing.After You (ChaosTokenFaceIsNot AutoFail))
          (ResourceCost 2)
    ]

instance RunMessage LuckyDice2 where
  runMessage msg a@(LuckyDice2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (Window.revealedChaosTokens -> [token]) _ -> do
      ignoreWindow <-
        checkWindows [mkWindow Timing.After (Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs)]
      pushAll
        [ CreateEffect "02230" Nothing (toSource attrs) (ChaosTokenTarget token)
        , DrawAnotherChaosToken iid
        , ignoreWindow
        ]
      pure a
    _ -> LuckyDice2 <$> runMessage msg attrs
