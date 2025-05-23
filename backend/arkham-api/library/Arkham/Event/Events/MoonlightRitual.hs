module Arkham.Event.Events.MoonlightRitual (moonlightRitual) where

import Arkham.Asset.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Event.Types (Field (..))
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype MoonlightRitual = MoonlightRitual EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlightRitual :: EventCard MoonlightRitual
moonlightRitual = event MoonlightRitual Cards.moonlightRitual

instance RunMessage MoonlightRitual where
  runMessage msg e@(MoonlightRitual attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      -- we assume that the only cards that are relevant here are assets,events, and investigators
      assets <- selectWithField AssetDoom $ assetControlledBy iid <> AssetWithAnyDoom
      events <- selectWithField EventDoom $ eventControlledBy iid <> EventWithAnyDoom
      investigatorDoomCount <- field InvestigatorDoom iid
      chooseOneM iid do
        when (investigatorDoomCount > 0) do
          targeting iid $ removeDoom attrs iid investigatorDoomCount
          for_ assets \(aid, assetDoomCount) ->
            targeting aid $ removeDoom attrs aid assetDoomCount
          for_ events \(eventId, eventDoomCount) ->
            targeting eventId $ removeDoom attrs eventId eventDoomCount
      pure e
    _ -> MoonlightRitual <$> liftRunMessage msg attrs
