module Arkham.Asset.Cards.ExpeditionJournal (
  expeditionJournal,
  ExpeditionJournal (..),
) where

import Arkham.Prelude

import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype ExpeditionJournal = ExpeditionJournal AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

expeditionJournal :: AssetCard ExpeditionJournal
expeditionJournal = asset ExpeditionJournal Cards.expeditionJournal

instance HasModifiersFor ExpeditionJournal where
  getModifiersFor (InvestigatorTarget iid) (ExpeditionJournal a) =
    pure
      $ toModifiers
        a
        [ GiveAdditionalAction $ AdditionalAction "Expedition Journal" (toSource a) #explore
        | controlledBy a iid
        ]
  getModifiersFor _ _ = pure []

instance RunMessage ExpeditionJournal where
  runMessage msg (ExpeditionJournal attrs) = ExpeditionJournal <$> runMessage msg attrs
