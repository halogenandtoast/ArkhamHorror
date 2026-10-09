module Arkham.Homebrew.TheSymphonyOfErichZann.Acts.MusicFromAuseilTheatre (musicFromAuseilTheatre) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Locations
import Arkham.Matcher

newtype MusicFromAuseilTheatre = MusicFromAuseilTheatre ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "*When the round ends*, investigators may spend the requisite number of
clues, as a group, to advance."

`ActAttrs`' own ability would offer the spend at any point during anyone's turn
(`restricted attrs 999 (DuringTurn Anyone) (Objective $ FastAbility cost)`), so
it is replaced here with the same cost on the round-end window. Still a
reaction, not a forced one: the investigators *may* spend.
-}
instance HasAbilities MusicFromAuseilTheatre where
  getAbilities (MusicFromAuseilTheatre a) =
    [ mkAbility a 1
        $ Objective
        $ ReactionAbility (RoundEnds #when) (GroupClueCost (PerPlayer 3) Anywhere) mempty
    ]

{- | "When the round ends, investigators may spend the requisite number of clues,
as a group, to advance."
-}
musicFromAuseilTheatre :: ActCard MusicFromAuseilTheatre
musicFromAuseilTheatre =
  act
    (1, A)
    MusicFromAuseilTheatre
    Cards.musicFromAuseilTheatre
    (groupClueCost $ PerPlayer 3)

instance RunMessage MusicFromAuseilTheatre where
  runMessage msg a@(MusicFromAuseilTheatre attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    -- The act's own b side is the Auguste Gaudin enemy, who spawns at the
    -- Auditorium as the act deck moves on.
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      auditorium <- selectJust $ locationIs Locations.auditorium
      createEnemyAt_ Enemies.augusteGaudinConductorOfTheVoid auditorium
      advanceActDeck attrs
      pure a
    _ -> MusicFromAuseilTheatre <$> liftRunMessage msg attrs
