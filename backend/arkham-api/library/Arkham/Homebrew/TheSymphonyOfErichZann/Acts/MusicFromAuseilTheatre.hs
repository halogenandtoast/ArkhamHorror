module Arkham.Homebrew.TheSymphonyOfErichZann.Acts.MusicFromAuseilTheatre (musicFromAuseilTheatre) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Locations
import Arkham.Matcher

newtype MusicFromAuseilTheatre = MusicFromAuseilTheatre ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "When the round ends, investigators may spend the requisite number of clues,
as a group, to advance."
-}
musicFromAuseilTheatre :: ActCard MusicFromAuseilTheatre
musicFromAuseilTheatre =
  act
    (1, A)
    MusicFromAuseilTheatre
    Cards.musicFromAuseilTheatre
    (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance RunMessage MusicFromAuseilTheatre where
  runMessage msg a@(MusicFromAuseilTheatre attrs) = runQueueT $ case msg of
    -- The act's own b side is the Auguste Gaudin enemy, who spawns at the
    -- Auditorium as the act deck moves on.
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      auditorium <- selectJust $ locationIs Locations.auditorium
      createEnemyAt_ Enemies.augusteGaudinConductorOfTheVoid auditorium
      advanceActDeck attrs
      pure a
    _ -> MusicFromAuseilTheatre <$> liftRunMessage msg attrs
