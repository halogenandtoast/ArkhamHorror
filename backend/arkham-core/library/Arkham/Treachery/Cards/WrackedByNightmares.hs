module Arkham.Treachery.Cards.WrackedByNightmares
  ( WrackedByNightmares(..)
  , wrackedByNightmares
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Source
import Arkham.Target
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype WrackedByNightmares = WrackedByNightmares TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wrackedByNightmares :: TreacheryCard WrackedByNightmares
wrackedByNightmares = treachery WrackedByNightmares Cards.wrackedByNightmares

instance HasModifiersFor WrackedByNightmares where
  getModifiersFor (InvestigatorTarget iid) (WrackedByNightmares attrs) =
    pure $ toModifiers
      attrs
      [ ControlledAssetsCannotReady | treacheryOnInvestigator iid attrs ]
  getModifiersFor _ _ = pure []

instance HasAbilities WrackedByNightmares where
  getAbilities (WrackedByNightmares a) =
    [ restrictedAbility a 1 OnSameLocation $ ActionAbility Nothing $ ActionCost
        2
    ]

instance RunMessage WrackedByNightmares where
  runMessage msg t@(WrackedByNightmares attrs@TreacheryAttrs {..}) =
    case msg of
      Revelation iid source | isSource attrs source -> do
        assetIds <- selectList (AssetControlledBy $ InvestigatorWithId iid)
        t <$ pushAll
          ([ Exhaust (AssetTarget aid) | aid <- assetIds ]
          <> [AttachTreachery treacheryId $ InvestigatorTarget iid]
          )
      UseCardAbility _ (TreacherySource tid) _ 1 _ | tid == treacheryId ->
        t <$ push (Discard (TreacheryTarget treacheryId))
      _ -> WrackedByNightmares <$> runMessage msg attrs
