module Arkham.Location.Cards.CrystalNursery (crystalNursery) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (codex, pattern Phi)
import Arkham.Card
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype CrystalNursery = CrystalNursery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystalNursery :: LocationCard CrystalNursery
crystalNursery = locationWith CrystalNursery Cards.crystalNursery 3 (PerPlayer 2) connectsToAdjacent

instance HasAbilities CrystalNursery where
  getAbilities (CrystalNursery a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 (Here <> exists (be a <> LocationWithCardsUnderneath AnyCards))
      $ triggered (TurnEnds #after You) (DirectHorrorCost (toSource a) You 1)

instance RunMessage CrystalNursery where
  runMessage msg l@(CrystalNursery attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      runMaybeT_ do
        loc <- MaybeT $ getLocationOf iid
        pos <- MaybeT $ field LocationPosition loc
        emptyPos <- MaybeT $ headMay <$> filterM (selectNone . LocationInPosition) pos.adjacents
        lift $ push $ PlaceGrid (GridLocation emptyPos attrs.id)
      codex iid attrs Phi
      CrystalNursery <$> liftRunMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ (headMay attrs.underneath) $ setFacedown False >=> drawCard iid
      pure l
    _ -> CrystalNursery <$> liftRunMessage msg attrs
