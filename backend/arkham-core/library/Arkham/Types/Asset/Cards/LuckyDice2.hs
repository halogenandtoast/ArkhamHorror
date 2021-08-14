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
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Window

newtype LuckyDice2 = LuckyDice2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luckyDice2 :: AssetCard LuckyDice2
luckyDice2 = accessory LuckyDice2 Cards.luckyDice2

ability :: AssetAttrs -> Token -> Ability
ability attrs token =
  (mkAbility (toSource attrs) 1 (ReactionAbility $ ResourceCost 2))
    { abilityMetadata = Just (TargetMetadata (TokenTarget token))
    }

instance HasAbilities env LuckyDice2 where
  getAbilities iid (AfterRevealToken who token) (LuckyDice2 a)
    | ownedBy a iid && who == iid && tokenFace token /= AutoFail = pure
      [ability a token]
  getAbilities _ _ _ = pure []

instance HasModifiersFor env LuckyDice2

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env LuckyDice2 where
  runMessage msg a@(LuckyDice2 attrs) = case msg of
    UseCardAbility iid source (Just (TargetMetadata target@(TokenTarget _))) 1 _
      | isSource attrs source -> a <$ pushAll
        [CreateEffect "02230" Nothing source target, DrawAnotherToken iid]
    _ -> LuckyDice2 <$> runMessage msg attrs
