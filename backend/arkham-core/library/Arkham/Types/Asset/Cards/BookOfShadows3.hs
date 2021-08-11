module Arkham.Types.Asset.Cards.BookOfShadows3
  ( BookOfShadows3(..)
  , bookOfShadows3
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait

newtype BookOfShadows3 = BookOfShadows3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bookOfShadows3 :: AssetCard BookOfShadows3
bookOfShadows3 = hand BookOfShadows3 Cards.bookOfShadows3

instance HasModifiersFor env BookOfShadows3

slot :: AssetAttrs -> Slot
slot AssetAttrs { assetId } = Slot (AssetSource assetId) Nothing

instance HasActions BookOfShadows3 where
  getActions (BookOfShadows3 a) =
    [ restrictedAbility a 1 OwnsThis $ ActionAbility Nothing $ Costs
        [ActionCost 1, ExhaustThis]
    ]

instance AssetRunner env => RunMessage env BookOfShadows3 where
  runMessage msg a@(BookOfShadows3 attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      push (AddSlot iid ArcaneSlot (slot attrs))
      BookOfShadows3 <$> runMessage msg attrs
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      spellAssetIds <- selectList (AssetOwnedBy You <> AssetWithTrait Spell)
      a <$ unless
        (null spellAssetIds)
        (push $ chooseOne
          iid
          [ AddUses (AssetTarget aid') Charge 1 | aid' <- spellAssetIds ]
        )
    _ -> BookOfShadows3 <$> runMessage msg attrs
