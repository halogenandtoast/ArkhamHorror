module Arkham.Asset.Cards.AshleighClarke (ashleighClarke, AshleighClarke (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Story.Cards qualified as Story

newtype AshleighClarke = AshleighClarke AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ashleighClarke :: AssetCard AshleighClarke
ashleighClarke = asset AshleighClarke Cards.ashleighClarke

instance HasAbilities AshleighClarke where
  getAbilities (AshleighClarke a) =
    [ restrictedAbility a 1 (OnSameLocation <> CanTakeControlOfClues)
        $ ActionAbility [#parley]
        $ ActionCost 2
    , mkAbility a 2 $ forced $ LastClueRemovedFromAsset #when $ AssetWithId $ toId a
    ]

instance RunMessage AshleighClarke where
  runMessage msg a@(AshleighClarke attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      when (assetClues attrs > 0)
        $ pushAll
          [ RemoveClues (attrs.ability 1) (toTarget attrs) 1
          , GainClues iid (attrs.ability 1) 1
          ]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      aboveAndBelow <- genCard Story.aboveAndBelow
      push $ ReadStory iid aboveAndBelow ResolveIt (Just $ toTarget attrs)
      pure a
    _ -> AshleighClarke <$> runMessage msg attrs
