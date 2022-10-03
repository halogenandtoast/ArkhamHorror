module Arkham.Asset.Cards.LuckyDice2
  ( luckyDice2
  , LuckyDice2(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype LuckyDice2 = LuckyDice2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luckyDice2 :: AssetCard LuckyDice2
luckyDice2 = asset LuckyDice2 Cards.luckyDice2

instance HasAbilities LuckyDice2 where
  getAbilities (LuckyDice2 a) =
    [ restrictedAbility a 1 ControlsThis $ ReactionAbility
        (RevealChaosToken Timing.After You (TokenFaceIsNot AutoFail))
        (ResourceCost 2)
    ]

instance RunMessage LuckyDice2 where
  runMessage msg a@(LuckyDice2 attrs) = case msg of
    UseCardAbility iid source 1 [Window _ (Window.RevealToken _ token)] _
      | isSource attrs source -> a <$ pushAll
        [ CreateEffect "02230" Nothing source (TokenTarget token)
        , DrawAnotherToken iid
        ]
    _ -> LuckyDice2 <$> runMessage msg attrs
