module Arkham.Types.Asset.Cards.LuckyDice2
  ( luckyDice2
  , LuckyDice2(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (After)
import Arkham.Types.Restriction
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Window
import qualified Arkham.Types.Window as Window

newtype LuckyDice2 = LuckyDice2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luckyDice2 :: AssetCard LuckyDice2
luckyDice2 = accessory LuckyDice2 Cards.luckyDice2

instance HasActions LuckyDice2 where
  getActions (LuckyDice2 a) =
    [ restrictedAbility a 1 OwnsThis $ ReactionAbility
        (RevealChaosToken After You (TokenFaceIsNot AutoFail))
        (ResourceCost 2)
    ]

instance HasModifiersFor env LuckyDice2

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env LuckyDice2 where
  runMessage msg a@(LuckyDice2 attrs) = case msg of
    UseCardAbility iid source [Window After (Window.RevealToken _ token)] 1 _
      | isSource attrs source -> a <$ pushAll
        [ CreateEffect "02230" Nothing source (TokenTarget token)
        , DrawAnotherToken iid
        ]
    _ -> LuckyDice2 <$> runMessage msg attrs
