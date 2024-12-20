module Arkham.Treachery.Cards.Doomed (doomed) where

import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Helpers.Log
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Doomed = Doomed TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doomed :: TreacheryCard Doomed
doomed = treachery Doomed Cards.doomed

instance RunMessage Doomed where
  runMessage msg t@(Doomed attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      doomApproached <- getHasRecord DoomApproaches
      if doomApproached
        then do
          accursedFate <- genPlayerCard Cards.accursedFate
          case toCard attrs of
            VengeanceCard _ -> error "not a vengeance card"
            EncounterCard _ -> error "not an encounter card"
            PlayerCard pc -> do
              assignHorror iid attrs 1
              removeCardFromDeckForCampaign iid pc
              addCampaignCardToDeck iid accursedFate
              putCardOnBottomOfDeck iid iid accursedFate
              removeTreachery attrs
        else do
          assignHorror iid attrs 1
          record DoomApproaches
      pure t
    _ -> Doomed <$> liftRunMessage msg attrs
