module Arkham.Types.Asset.Cards.LuckyDice2
  ( luckyDice2
  , LuckyDice2(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Token
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as Window

newtype LuckyDice2 = LuckyDice2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luckyDice2 :: AssetCard LuckyDice2
luckyDice2 = accessory LuckyDice2 Cards.luckyDice2

instance HasAbilities env LuckyDice2 where
  getAbilities _ _ (LuckyDice2 a) = pure
    [ restrictedAbility a 1 OwnsThis $ ReactionAbility
        (RevealChaosToken Timing.After You (TokenFaceIsNot AutoFail))
        (ResourceCost 2)
    ]

instance AssetRunner env => RunMessage env LuckyDice2 where
  runMessage msg a@(LuckyDice2 attrs) = case msg of
    UseCardAbility iid source [Window _ (Window.RevealToken _ token)] 1 _
      | isSource attrs source -> a <$ pushAll
        [ CreateEffect "02230" Nothing source (TokenTarget token)
        , DrawAnotherToken iid
        ]
    _ -> LuckyDice2 <$> runMessage msg attrs
