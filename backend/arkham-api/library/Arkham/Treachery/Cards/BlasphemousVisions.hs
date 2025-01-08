module Arkham.Treachery.Cards.BlasphemousVisions (blasphemousVisions) where

import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Card
import Arkham.EncounterSet (EncounterSet (Tekelili))
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Matcher.Card
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BlasphemousVisions = BlasphemousVisions TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blasphemousVisions :: TreacheryCard BlasphemousVisions
blasphemousVisions = treachery BlasphemousVisions Cards.blasphemousVisions

instance HasModifiersFor BlasphemousVisions where
  getModifiersFor (BlasphemousVisions a) = do
    cards <- findAllCards (`cardMatch` (CardFromEncounterSet Tekelili))
    for_ cards \card -> modified_ a card [ResolveEffectsAgain]

instance RunMessage BlasphemousVisions where
  runMessage msg t@(BlasphemousVisions attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      cards <- getTekelili 1
      unless (null cards) do
        addTekelili iid cards
        placeInThreatArea attrs iid
      pure t
    _ -> BlasphemousVisions <$> liftRunMessage msg attrs
