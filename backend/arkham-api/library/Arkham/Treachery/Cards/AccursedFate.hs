module Arkham.Treachery.Cards.AccursedFate (accursedFate) where

import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Helpers.Log
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AccursedFate = AccursedFate TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

accursedFate :: TreacheryCard AccursedFate
accursedFate = treachery AccursedFate Cards.accursedFate

instance RunMessage AccursedFate where
  runMessage msg t@(AccursedFate attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      theHourIsNight <- getHasRecord TheHourIsNigh
      if theHourIsNight
        then do
          theBellTolls <- genPlayerCard Cards.theBellTolls
          case toCard attrs of
            EncounterCard _ -> error "not an encounter card"
            VengeanceCard _ -> error "not a vengeance card"
            PlayerCard pc -> do
              assignHorror iid attrs 2
              removeCardFromDeckForCampaign iid pc
              addCampaignCardToDeck iid theBellTolls
              putCardOnBottomOfDeck iid iid theBellTolls
              removeTreachery attrs
        else do
          assignHorror iid attrs 2
          record TheHourIsNigh

      pure t
    _ -> AccursedFate <$> liftRunMessage msg attrs
