module Arkham.Location.Cards.SaltChamber (saltChamber) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (codex, pattern Omega)
import Arkham.Card
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype SaltChamber = SaltChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

saltChamber :: LocationCard SaltChamber
saltChamber = locationWith SaltChamber Cards.saltChamber 4 (PerPlayer 3) connectsToAdjacent

instance HasAbilities SaltChamber where
  getAbilities (SaltChamber a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 (Here <> exists (be a <> LocationWithCardsUnderneath AnyCards))
      $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) (be a))

instance RunMessage SaltChamber where
  runMessage msg l@(SaltChamber attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      runMaybeT_ do
        loc <- MaybeT $ getLocationOf iid
        pos <- MaybeT $ field LocationPosition loc
        emptyPos <- MaybeT $ headMay <$> filterM (selectNone . LocationInPosition) pos.adjacents
        lift $ push $ PlaceGrid (GridLocation emptyPos attrs.id)
      codex iid attrs Omega
      SaltChamber <$> liftRunMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ (headMay attrs.underneath) $ setFacedown False >=> drawCard iid
      pure l
    _ -> SaltChamber <$> liftRunMessage msg attrs
