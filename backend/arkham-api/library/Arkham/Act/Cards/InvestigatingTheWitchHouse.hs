module Arkham.Act.Cards.InvestigatingTheWitchHouse (investigatingTheWitchHouse) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Constants
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Location
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose

newtype InvestigatingTheWitchHouse = InvestigatingTheWitchHouse ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

investigatingTheWitchHouse :: ActCard InvestigatingTheWitchHouse
investigatingTheWitchHouse = act (1, A) InvestigatingTheWitchHouse Cards.investigatingTheWitchHouse Nothing

instance HasAbilities InvestigatingTheWitchHouse where
  getAbilities = actAbilities1 \a ->
    restricted
      a
      ActAdvancement
      (DuringTurn Anyone <> EachUndefeatedInvestigator (at_ $ locationIs Locations.walterGilmansRoom))
      $ Objective (FastAbility $ GroupClueCost (PerPlayer 3) $ locationIs Locations.walterGilmansRoom)

instance RunMessage InvestigatingTheWitchHouse where
  runMessage msg a@(InvestigatingTheWitchHouse attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lid <- selectJust $ locationIs Locations.walterGilmansRoom
      keziahsRoom <- getSetAsideCard Locations.keziahsRoom
      swapLocation lid keziahsRoom

      selectEach (not_ $ LocationWithId lid) removeLocation

      iids <- getInvestigators
      theBlackBook <- getSetAsideCard Assets.theBlackBook
      leadChooseOneM $ targets iids (`takeControlOfSetAsideAsset` theBlackBook)

      strangeGeometries <- getSetAsideCardsMatching (CardWithTitle "Strange Geometry")
      shuffleCardsIntoDeck Deck.EncounterDeck strangeGeometries
      advanceActDeck attrs
      pure a
    _ -> InvestigatingTheWitchHouse <$> liftRunMessage msg attrs
