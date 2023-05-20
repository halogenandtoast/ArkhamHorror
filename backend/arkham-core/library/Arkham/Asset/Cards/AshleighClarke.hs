module Arkham.Asset.Cards.AshleighClarke (
  ashleighClarke,
  AshleighClarke (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.Story.Cards qualified as Story
import Arkham.Timing qualified as Timing

newtype AshleighClarke = AshleighClarke AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ashleighClarke :: AssetCard AshleighClarke
ashleighClarke = asset AshleighClarke Cards.ashleighClarke

instance HasAbilities AshleighClarke where
  getAbilities (AshleighClarke a) =
    [ restrictedAbility a 1 (OnSameLocation <> CanTakeControlOfClues) $
        ActionAbility Nothing $
          ActionCost 2
    , mkAbility a 2 $
        ForcedAbility $
          LastClueRemovedFromAsset Timing.When $
            AssetWithId $
              toId a
    ]

instance RunMessage AshleighClarke where
  runMessage msg a@(AshleighClarke attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      when (assetClues attrs > 0) $
        pushAll
          [ RemoveClues (toAbilitySource attrs 1) (toTarget attrs) 1
          , GainClues iid (toAbilitySource attrs 1) 1
          ]
      pure a
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      aboveAndBelow <- genCard Story.aboveAndBelow
      push $ ReadStory iid aboveAndBelow ResolveIt (Just $ toTarget attrs)
      pure a
    _ -> AshleighClarke <$> runMessage msg attrs
