module Arkham.Types.Asset.Cards.Newspaper
  ( newspaper
  , Newspaper(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Newspaper = Newspaper AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newspaper :: AssetCard Newspaper
newspaper = hand Newspaper Cards.newspaper

instance HasActions Newspaper

instance HasCount ClueCount env InvestigatorId => HasModifiersFor env Newspaper where
  getModifiersFor _ (InvestigatorTarget iid) (Newspaper a) | ownedBy a iid = do
    clueCount <- unClueCount <$> getCount iid
    pure
      [ toModifier a $ ActionSkillModifier Action.Investigate SkillIntellect 2
      | clueCount == 0
      ]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env Newspaper where
  runMessage msg (Newspaper attrs) = Newspaper <$> runMessage msg attrs
