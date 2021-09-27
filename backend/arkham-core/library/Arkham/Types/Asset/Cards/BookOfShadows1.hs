module Arkham.Types.Asset.Cards.BookOfShadows1
  ( bookOfShadows1
  , BookOfShadows1(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait

newtype BookOfShadows1 = BookOfShadows1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bookOfShadows1 :: AssetCard BookOfShadows1
bookOfShadows1 = asset BookOfShadows1 Cards.bookOfShadows1

instance HasAbilities BookOfShadows1 where
  getAbilities (BookOfShadows1 a) =
    [ restrictedAbility a 1 OwnsThis $ ActionAbility Nothing $ Costs
        [ActionCost 1, ResourceCost 1, ExhaustCost (toTarget a)]
    ]

instance AssetRunner env => RunMessage env BookOfShadows1 where
  runMessage msg a@(BookOfShadows1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      spellAssetIds <- selectList (AssetOwnedBy You <> AssetWithTrait Spell)
      a <$ unless
        (null spellAssetIds)
        (push $ chooseOne
          iid
          [ AddUses (AssetTarget aid') Charge 1 | aid' <- spellAssetIds ]
        )
    _ -> BookOfShadows1 <$> runMessage msg attrs
