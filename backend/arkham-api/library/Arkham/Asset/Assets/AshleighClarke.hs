module Arkham.Asset.Assets.AshleighClarke (ashleighClarke) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Story
import Arkham.Matcher
import Arkham.Story.Cards qualified as Story

newtype AshleighClarke = AshleighClarke AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ashleighClarke :: AssetCard AshleighClarke
ashleighClarke = asset AshleighClarke Cards.ashleighClarke

instance HasAbilities AshleighClarke where
  getAbilities (AshleighClarke a) =
    [ restricted a 1 (OnSameLocation <> CanTakeControlOfClues) $ ActionAbility [#parley] $ ActionCost 2
    , groupLimit PerGame
        $ restricted a 2 (not_ $ exists Story.sickeningReality_69)
        $ forced
        $ LastClueRemovedFromAsset #when (be a)
    ]

instance RunMessage AshleighClarke where
  runMessage msg a@(AshleighClarke attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      when (attrs.token #clue > 0) $ moveTokens (attrs.ability 1) attrs iid #clue 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      readStory iid attrs Story.aboveAndBelow
      pure a
    _ -> AshleighClarke <$> liftRunMessage msg attrs
