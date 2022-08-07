module Arkham.Asset.Cards.CharonsObol1
  ( charonsObol1
  , CharonsObol1(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Target

newtype CharonsObol1 = CharonsObol1 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor CharonsObol1 where
  getModifiersFor (InvestigatorTarget iid) (CharonsObol1 attrs)
    | controlledBy attrs iid = do
      isDefeated <- member iid <$> select DefeatedInvestigator
      pure
        $ toModifiers attrs
        $ KilledIfDefeated
        : [ XPModifier 2 | not isDefeated ]
  getModifiersFor _ _ = pure []

charonsObol1 :: AssetCard CharonsObol1
charonsObol1 = asset CharonsObol1 Cards.charonsObol1

instance RunMessage CharonsObol1 where
  runMessage msg (CharonsObol1 attrs) = CharonsObol1 <$> runMessage msg attrs
