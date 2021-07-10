module Arkham.Types.Asset.Cards.BookOfShadows3
  ( BookOfShadows3(..)
  , bookOfShadows3
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait

newtype BookOfShadows3 = BookOfShadows3 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bookOfShadows3 :: AssetCard BookOfShadows3
bookOfShadows3 = hand BookOfShadows3 Cards.bookOfShadows3

instance HasModifiersFor env BookOfShadows3

slot :: AssetAttrs -> Slot
slot AssetAttrs { assetId } = Slot (AssetSource assetId) Nothing

instance HasAbilities BookOfShadows3 where
  getAbilities (BookOfShadows3 a) =
    [assetAction a 1 Nothing $ Costs [ActionCost 1, ExhaustCost (toTarget a)]]

instance AssetRunner env => RunMessage env BookOfShadows3 where
  runMessage msg a@(BookOfShadows3 attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      push (AddSlot iid ArcaneSlot (slot attrs))
      BookOfShadows3 <$> runMessage msg attrs
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      assetIds <- getSetList iid
      spellAssetIds <- filterM ((member Spell <$>) . getSet) assetIds
      a <$ unless
        (null spellAssetIds)
        (push $ chooseOne
          iid
          [ AddUses (AssetTarget aid') Charge 1 | aid' <- spellAssetIds ]
        )
    _ -> BookOfShadows3 <$> runMessage msg attrs
