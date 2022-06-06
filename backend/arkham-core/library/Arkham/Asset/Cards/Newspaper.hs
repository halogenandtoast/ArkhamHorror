module Arkham.Asset.Cards.Newspaper
  ( newspaper
  , Newspaper(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Runner
import Arkham.Id
import Arkham.Modifier
import Arkham.Query
import Arkham.SkillType
import Arkham.Target

newtype Newspaper = Newspaper AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newspaper :: AssetCard Newspaper
newspaper = asset Newspaper Cards.newspaper

instance HasCount ClueCount env InvestigatorId => HasModifiersFor env Newspaper where
  getModifiersFor _ (InvestigatorTarget iid) (Newspaper a) | controlledBy a iid = do
    clueCount <- unClueCount <$> getCount iid
    pure
      [ toModifier a $ ActionSkillModifier Action.Investigate SkillIntellect 2
      | clueCount == 0
      ]
  getModifiersFor _ _ _ = pure []

instance RunMessage Newspaper where
  runMessage msg (Newspaper attrs) = Newspaper <$> runMessage msg attrs
