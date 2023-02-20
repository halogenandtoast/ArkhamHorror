module Arkham.Asset.Cards.ExpeditionJournal
  ( expeditionJournal
  , ExpeditionJournal(..)
  ) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Target

newtype ExpeditionJournal = ExpeditionJournal AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

expeditionJournal :: AssetCard ExpeditionJournal
expeditionJournal = asset ExpeditionJournal Cards.expeditionJournal

instance HasModifiersFor ExpeditionJournal where
  getModifiersFor (InvestigatorTarget iid) (ExpeditionJournal a) =
    pure $ toModifiers
      a
      [ GiveAdditionalAction $ ActionRestrictedAdditionalAction Action.Explore
      | controlledBy a iid
      ]
  getModifiersFor _ _ = pure []

instance RunMessage ExpeditionJournal where
  runMessage msg a@(ExpeditionJournal attrs) = case msg of
    InvestigatorPlayAsset iid assetId | toId attrs == assetId -> do
      push
        $ GainAdditionalAction iid (toSource attrs)
        $ ActionRestrictedAdditionalAction Action.Explore
      pure a
    _ -> ExpeditionJournal <$> runMessage msg attrs
