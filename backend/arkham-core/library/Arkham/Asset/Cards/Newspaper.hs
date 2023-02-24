module Arkham.Asset.Cards.Newspaper
  ( newspaper
  , Newspaper(..)
  ) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Projection
import Arkham.SkillType

newtype Newspaper = Newspaper AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newspaper :: AssetCard Newspaper
newspaper = asset Newspaper Cards.newspaper

instance HasModifiersFor Newspaper where
  getModifiersFor (InvestigatorTarget iid) (Newspaper a)
    | controlledBy a iid = do
      clueCount <- field InvestigatorClues iid
      pure
        [ toModifier a $ ActionSkillModifier Action.Investigate SkillIntellect 2
        | clueCount == 0
        ]
  getModifiersFor _ _ = pure []

instance RunMessage Newspaper where
  runMessage msg (Newspaper attrs) = Newspaper <$> runMessage msg attrs
