module Arkham.Location.Cards.LarvalTunnel (larvalTunnel) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (codex, pattern Psi)
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype LarvalTunnel = LarvalTunnel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

larvalTunnel :: LocationCard LarvalTunnel
larvalTunnel = locationWith LarvalTunnel Cards.larvalTunnel 2 (PerPlayer 3) connectsToAdjacent

instance HasAbilities LarvalTunnel where
  getAbilities (LarvalTunnel a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 (Here <> exists (be a <> LocationWithCardsUnderneath AnyCards))
      $ freeReaction
      $ EnemyEvadedSuccessfully #after You AnySource (at_ $ be a)

instance RunMessage LarvalTunnel where
  runMessage msg l@(LarvalTunnel attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      runMaybeT_ do
        loc <- MaybeT $ getLocationOf iid
        pos <- MaybeT $ field LocationPosition loc
        emptyPos <- MaybeT $ headMay <$> filterM (selectNone . LocationInPosition) pos.adjacents
        lift $ push $ PlaceGrid (GridLocation emptyPos attrs.id)
      findEncounterCard iid attrs $ cardIs Enemies.colorlessLarva
      codex iid attrs Psi
      LarvalTunnel <$> liftRunMessage msg attrs
    FoundEncounterCard _iid (isTarget attrs -> True) card -> do
      createEnemyAt_ card attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ (headMay attrs.underneath) $ setFacedown False >=> drawCard iid
      pure l
    _ -> LarvalTunnel <$> liftRunMessage msg attrs
