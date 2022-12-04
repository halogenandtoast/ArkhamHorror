module Arkham.Asset.Cards.BookOfShadows1
  ( bookOfShadows1
  , BookOfShadows1(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Trait

newtype BookOfShadows1 = BookOfShadows1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bookOfShadows1 :: AssetCard BookOfShadows1
bookOfShadows1 = asset BookOfShadows1 Cards.bookOfShadows1

instance HasAbilities BookOfShadows1 where
  getAbilities (BookOfShadows1 a) =
    [ restrictedAbility
          a
          1
          (ControlsThis
          <> AssetExists (AssetControlledBy You <> AssetWithTrait Spell)
          )
        $ ActionAbility Nothing
        $ Costs [ActionCost 1, ResourceCost 1, ExhaustCost (toTarget a)]
    ]

instance RunMessage BookOfShadows1 where
  runMessage msg a@(BookOfShadows1 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      spellAssetIds <- selectList
        (AssetControlledBy You <> AssetWithTrait Spell)
      unless (null spellAssetIds) $ push $ chooseOne
        iid
        [ targetLabel aid' [AddUses aid' Charge 1]
        | aid' <- spellAssetIds
        ]
      pure a
    _ -> BookOfShadows1 <$> runMessage msg attrs
