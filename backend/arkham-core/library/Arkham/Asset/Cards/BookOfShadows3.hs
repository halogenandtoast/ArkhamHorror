module Arkham.Asset.Cards.BookOfShadows3
  ( BookOfShadows3(..)
  , bookOfShadows3
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Target
import Arkham.Trait

newtype BookOfShadows3 = BookOfShadows3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bookOfShadows3 :: AssetCard BookOfShadows3
bookOfShadows3 = asset BookOfShadows3 Cards.bookOfShadows3

slot :: AssetAttrs -> Slot
slot attrs = Slot (toSource attrs) Nothing

instance HasAbilities BookOfShadows3 where
  getAbilities (BookOfShadows3 a) =
    [ restrictedAbility
          a
          1
          (ControlsThis
          <> AssetExists (AssetControlledBy You <> AssetWithTrait Spell)
          )
        $ ActionAbility Nothing
        $ Costs [ActionCost 1, ExhaustCost (toTarget a)]
    ]

instance RunMessage BookOfShadows3 where
  runMessage msg a@(BookOfShadows3 attrs) = case msg of
    InvestigatorPlayAsset iid aid | aid == assetId attrs -> do
      push (AddSlot iid ArcaneSlot (slot attrs))
      BookOfShadows3 <$> runMessage msg attrs
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      spellAssetIds <- selectList
        (AssetControlledBy You <> AssetWithTrait Spell)
      a <$ unless
        (null spellAssetIds)
        (push $ chooseOne
          iid
          [ TargetLabel (AssetTarget aid') [AddUses (AssetTarget aid') Charge 1]
          | aid' <- spellAssetIds
          ]
        )
    _ -> BookOfShadows3 <$> runMessage msg attrs
