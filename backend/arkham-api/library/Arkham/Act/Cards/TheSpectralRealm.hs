module Arkham.Act.Cards.TheSpectralRealm (
  TheSpectralRealm (..),
  theSpectralRealm,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Matcher hiding (RevealLocation)

newtype TheSpectralRealm = TheSpectralRealm ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theSpectralRealm :: ActCard TheSpectralRealm
theSpectralRealm =
  act
    (2, A)
    TheSpectralRealm
    Cards.theSpectralRealm
    (Just $ GroupClueCost (PerPlayer 4) Anywhere)

instance RunMessage TheSpectralRealm where
  runMessage msg a@(TheSpectralRealm attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId a && onSide B attrs -> do
      entryHallId <- getJustLocationByName "Entry Hall"
      pushAll
        [ RevealLocation Nothing entryHallId
        , ShuffleEncounterDiscardBackIn
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> TheSpectralRealm <$> runMessage msg attrs
