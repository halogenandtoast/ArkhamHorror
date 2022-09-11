module Arkham.Asset.Cards.ExpeditionJournal
  ( expeditionJournal
  , ExpeditionJournal(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype ExpeditionJournal = ExpeditionJournal AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

expeditionJournal :: AssetCard ExpeditionJournal
expeditionJournal = asset ExpeditionJournal Cards.expeditionJournal

instance RunMessage ExpeditionJournal where
  runMessage msg (ExpeditionJournal attrs) =
    ExpeditionJournal <$> runMessage msg attrs
