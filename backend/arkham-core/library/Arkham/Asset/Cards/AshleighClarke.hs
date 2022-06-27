module Arkham.Asset.Cards.AshleighClarke
  ( ashleighClarke
  , AshleighClarke(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Story.Cards qualified as Story
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype AshleighClarke = AshleighClarke AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ashleighClarke :: AssetCard AshleighClarke
ashleighClarke = asset AshleighClarke Cards.ashleighClarke

instance HasAbilities AshleighClarke where
  getAbilities (AshleighClarke a) =
    [ restrictedAbility a 1 (OnSameLocation <> CanTakeControlOfClues)
      $ ActionAbility Nothing
      $ ActionCost 2
    , mkAbility a 2
      $ ForcedAbility
      $ LastClueRemovedFromAsset Timing.When
      $ AssetWithId
      $ toId a
    ]

instance RunMessage AshleighClarke where
  runMessage msg a@(AshleighClarke attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ when
      (assetClues attrs > 0)
      (pushAll [RemoveClues (toTarget attrs) 1, GainClues iid 1])
    UseCardAbility iid source _ 2 _ | isSource attrs source -> do
      a <$ push (ReadStory iid Story.aboveAndBelow)
    _ -> AshleighClarke <$> runMessage msg attrs
