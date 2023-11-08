module Arkham.Treachery.Cards.WrackedByNightmares (
  WrackedByNightmares (..),
  wrackedByNightmares,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype WrackedByNightmares = WrackedByNightmares TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wrackedByNightmares :: TreacheryCard WrackedByNightmares
wrackedByNightmares = treachery WrackedByNightmares Cards.wrackedByNightmares

instance HasModifiersFor WrackedByNightmares where
  getModifiersFor (InvestigatorTarget iid) (WrackedByNightmares attrs) =
    pure
      $ toModifiers
        attrs
        [ControlledAssetsCannotReady | treacheryOnInvestigator iid attrs]
  getModifiersFor _ _ = pure []

instance HasAbilities WrackedByNightmares where
  getAbilities (WrackedByNightmares a) =
    [ restrictedAbility a 1 OnSameLocation
        $ ActionAbility []
        $ ActionCost
          2
    ]

instance RunMessage WrackedByNightmares where
  runMessage msg t@(WrackedByNightmares attrs@TreacheryAttrs {..}) =
    case msg of
      Revelation iid source | isSource attrs source -> do
        assetIds <- selectList (AssetControlledBy $ InvestigatorWithId iid)
        pushAll
          $ [Exhaust (AssetTarget aid) | aid <- assetIds]
          <> [AttachTreachery treacheryId $ InvestigatorTarget iid]
        pure t
      UseCardAbility iid (TreacherySource tid) 1 _ _ | tid == treacheryId -> do
        t <$ push (toDiscardBy iid (toAbilitySource attrs 1) treacheryId)
      _ -> WrackedByNightmares <$> runMessage msg attrs
