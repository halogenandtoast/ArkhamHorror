module Arkham.Asset.Assets.EliasHusserlJrFleeingTheCity (eliasHusserlJrFleeingTheCity) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Location.CardDefs.TracesToNowhere qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Trait (Trait (Syndicate))

newtype EliasHusserlJrFleeingTheCity = EliasHusserlJrFleeingTheCity AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eliasHusserlJrFleeingTheCity :: AssetCard EliasHusserlJrFleeingTheCity
eliasHusserlJrFleeingTheCity = allyWith EliasHusserlJrFleeingTheCity Cards.eliasHusserlJrFleeingTheCity (2, 2) noSlots

instance HasAbilities EliasHusserlJrFleeingTheCity where
  getAbilities (EliasHusserlJrFleeingTheCity a) = [restricted a 1 OnSameLocation parleyAction_]

instance RunMessage EliasHusserlJrFleeingTheCity where
  runMessage msg a@(EliasHusserlJrFleeingTheCity attrs) = runQueueT $ scenarioI18n $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeLocationIfNotInPlay_ Locations.trainStation
      selectForMaybeM (locationIs Locations.trainStation) $ place attrs . AtLocation
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codexEntry "codex.eliasHusserlJr"
      withLocationOf attrs \lid -> do
        -- "Put the set-aside Eugene Peel story asset into play at Elias Husserl Jr.'s location."
        createAssetAt_ Cards.eugenePeelHidingSomething (AtLocation lid)
        {- "Search the encounter deck and discard pile for 1 {perPlayer} Syndicate
        enemies and spawn them at Elias Husserl Jr.'s location." Each search is its
        own message, and they resolve ahead of the victory push below, so Elias is
        still in play to spawn beside. -}
        n <- getPlayerCount
        replicateM_ n $ findEncounterCard iid attrs (#enemy <> CardWithTrait Syndicate)
      addToVictory_ attrs
      pure a
    FoundEncounterCard _ (isTarget attrs -> True) card -> do
      withLocationOf attrs $ spawnEnemyAt_ card
      pure a
    _ -> EliasHusserlJrFleeingTheCity <$> liftRunMessage msg attrs
