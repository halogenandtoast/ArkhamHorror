module Arkham.Asset.Assets.CeremonialRobes1 (ceremonialRobes1) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.History
import Arkham.Matcher
import Arkham.Trait (Trait (Ritual, Spell))

newtype CeremonialRobes1 = CeremonialRobes1 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ceremonialRobes1 :: AssetCard CeremonialRobes1
ceremonialRobes1 = assetWith CeremonialRobes1 Cards.ceremonialRobes1 ((healthL ?~ 1) . (sanityL ?~ 1))

instance HasModifiersFor CeremonialRobes1 where
  getModifiersFor (CeremonialRobes1 a) = for_ a.controller \iid -> do
    maybeModified_ a iid do
      playedCards <- lift $ historyPlayedCards <$> getHistory RoundHistory iid
      let cardMatcher = mapOneOf CardWithTrait [Spell, Ritual]
      guard $ none (`cardMatch` cardMatcher) playedCards
      pure [ReduceCostOf cardMatcher 1]
