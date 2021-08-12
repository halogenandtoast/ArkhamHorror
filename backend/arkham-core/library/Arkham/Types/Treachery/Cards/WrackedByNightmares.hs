module Arkham.Types.Treachery.Cards.WrackedByNightmares
  ( WrackedByNightmares(..)
  , wrackedByNightmares
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype WrackedByNightmares = WrackedByNightmares TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wrackedByNightmares :: TreacheryCard WrackedByNightmares
wrackedByNightmares = treachery WrackedByNightmares Cards.wrackedByNightmares

instance HasModifiersFor env WrackedByNightmares where
  getModifiersFor _ (InvestigatorTarget iid) (WrackedByNightmares attrs) =
    pure $ toModifiers
      attrs
      [ ControlledAssetsCannotReady | treacheryOnInvestigator iid attrs ]
  getModifiersFor _ _ _ = pure []

instance HasActions WrackedByNightmares where
  getActions (WrackedByNightmares a) =
    [ restrictedAbility a 1 OnSameLocation $ ActionAbility Nothing $ ActionCost
        2
    ]

instance TreacheryRunner env => RunMessage env WrackedByNightmares where
  runMessage msg t@(WrackedByNightmares attrs@TreacheryAttrs {..}) =
    case msg of
      Revelation iid source | isSource attrs source ->
        t <$ push (AttachTreachery treacheryId $ InvestigatorTarget iid)
      AttachTreachery tid (InvestigatorTarget iid) | tid == treacheryId -> do
        assetIds <- selectList (AssetOwnedBy $ InvestigatorWithId iid)
        pushAll [ Exhaust (AssetTarget aid) | aid <- assetIds ]
        WrackedByNightmares <$> runMessage msg attrs
      UseCardAbility _ (TreacherySource tid) _ 1 _ | tid == treacheryId ->
        t <$ push (Discard (TreacheryTarget treacheryId))
      _ -> WrackedByNightmares <$> runMessage msg attrs
