module Arkham.Asset.Assets.ExpeditionJournal (expeditionJournal, ExpeditionJournal (..)) where

import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)

newtype ExpeditionJournal = ExpeditionJournal AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

expeditionJournal :: AssetCard ExpeditionJournal
expeditionJournal = asset ExpeditionJournal Cards.expeditionJournal

instance HasModifiersFor ExpeditionJournal where
  getModifiersFor (ExpeditionJournal a) =
    controllerGets
      a
      [GiveAdditionalAction $ AdditionalAction "Expedition Journal" (toSource a) #explore]

instance RunMessage ExpeditionJournal where
  runMessage msg (ExpeditionJournal attrs) = ExpeditionJournal <$> runMessage msg attrs
