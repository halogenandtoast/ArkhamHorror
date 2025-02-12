module Arkham.Asset.Assets.BookOfVerseUnCommonplaceBook (bookOfVerseUnCommonplaceBook) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.History
import Arkham.Matcher hiding (DiscoverClues)
import Arkham.Modifier

newtype BookOfVerseUnCommonplaceBook = BookOfVerseUnCommonplaceBook AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bookOfVerseUnCommonplaceBook :: AssetCard BookOfVerseUnCommonplaceBook
bookOfVerseUnCommonplaceBook = asset BookOfVerseUnCommonplaceBook Cards.bookOfVerseUnCommonplaceBook

instance HasAbilities BookOfVerseUnCommonplaceBook where
  getAbilities (BookOfVerseUnCommonplaceBook a) =
    [ restricted a 1 ControlsThis
        $ triggered (RevealChaosTokensDuringSkillTest #after You #any #any) (assetUseCost a Inspiration 1)
    ]

instance RunMessage BookOfVerseUnCommonplaceBook where
  runMessage msg a@(BookOfVerseUnCommonplaceBook attrs) = runQueueT $ case msg of
    Do (DiscoverClues iid d) | attrs `controlledBy` iid && d.count > 0 -> do
      history <- sum . toList <$> getHistoryField #round iid HistoryCluesDiscovered
      pure do
        if history == 0
          then BookOfVerseUnCommonplaceBook $ attrs & tokensL %~ replenish Inspiration 1
          else a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 1)
        onSucceedByEffect sid (static 0) (attrs.ability 1) sid $ doStep 1 msg
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      drawCards iid (attrs.ability 1) 1
      pure a
    _ -> BookOfVerseUnCommonplaceBook <$> liftRunMessage msg attrs
