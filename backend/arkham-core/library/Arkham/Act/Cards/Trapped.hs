module Arkham.Act.Cards.Trapped where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Location.Cards qualified as Locations

newtype Trapped = Trapped ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

trapped :: ActCard Trapped
trapped = act (1, A) Trapped Cards.trapped (groupClueCost $ PerPlayer 2)

instance RunMessage Trapped where
  runMessage msg a@(Trapped attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      study <- getJustLocationByName "Study"
      enemies <- enemiesAt study

      (hallway, placeHallway) <- placeSetAsideLocation Locations.hallway
      placeCellar <- placeSetAsideLocation_ Locations.cellar
      placeAttic <- placeSetAsideLocation_ Locations.attic
      placeParlor <- placeSetAsideLocation_ Locations.parlor

      pushAll
        $ [placeHallway, placeCellar, placeAttic, placeParlor]
        <> map (toDiscard attrs) enemies
        <> [ RevealLocation Nothing hallway
           , MoveAllTo (toSource attrs) hallway
           , RemoveLocation study
           , advanceActDeck attrs
           ]
      pure a
    _ -> Trapped <$> runMessage msg attrs
