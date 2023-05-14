module Arkham.Act.Cards.InPursuitOfTheDead (
  InPursuitOfTheDead (..),
  inPursuitOfTheDead,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Trait (Trait (Spectral))

newtype InPursuitOfTheDead = InPursuitOfTheDead ActAttrs
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- Errata: The text on this card should read: "Locations cannot be flipped to their spectral side"
--
instance HasModifiersFor InPursuitOfTheDead where
  getModifiersFor (LocationTarget lid) (InPursuitOfTheDead attrs) = do
    notSpectral <- lid <=~> NotLocation (LocationWithTrait Spectral)
    pure $ toModifiers attrs [CannotBeFlipped | notSpectral]
  getModifiersFor _ _ = pure []

inPursuitOfTheDead :: ActCard InPursuitOfTheDead
inPursuitOfTheDead =
  act (1, A) InPursuitOfTheDead Cards.inPursuitOfTheDead (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance RunMessage InPursuitOfTheDead where
  runMessage msg a@(InPursuitOfTheDead attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      heretics <- getSetAsideCardsMatching $ CardWithTitle "Heretic"
      case heretics of
        [heretic1, heretic2, heretic3, heretic4] -> do
          theGallows <- getJustLocationIdByName "The Gallows"
          hereticsGraves <- getJustLocationIdByName "Heretics' Graves"
          chapelAttic <- getJustLocationIdByName "Chapel Attic"
          chapelCrypt <- getJustLocationIdByName "Chapel Crypt"

          createHeretic1 <- createEnemyAt_ heretic1 theGallows Nothing
          createHeretic2 <- createEnemyAt_ heretic2 hereticsGraves Nothing
          createHeretic3 <- createEnemyAt_ heretic3 chapelAttic Nothing
          createHeretic4 <- createEnemyAt_ heretic4 chapelCrypt Nothing
          onePerPlayer <- getPlayerCountValue (PerPlayer 1)
          twoPerPlayer <- getPlayerCountValue (PerPlayer 2)

          let hereticLocations = [theGallows, hereticsGraves, chapelAttic, chapelCrypt]

          otherLocations <-
            selectList $
              locationNotOneOf hereticLocations

          mementosDiscovered <- getMementosDiscoveredCount

          when (mementosDiscovered >= 3) $ send "\"You understand the tragic lyrics behind the witch's song\""
          spectralWebs <- getSetAsideCardsMatching $ cardIs Assets.spectralWeb
          investigators <- getInvestigatorIds

          pushAll $
            [createHeretic1, createHeretic2, createHeretic3, createHeretic4]
              <> [PlaceClues (toTarget lid) twoPerPlayer | lid <- hereticLocations]
              <> [PlaceClues (toTarget lid) onePerPlayer | lid <- otherLocations]
              <> zipWith TakeControlOfSetAsideAsset investigators spectralWebs
              <> [advanceActDeck attrs]
        _ -> error "Invalid number of heretics"
      pure a
    _ -> InPursuitOfTheDead <$> runMessage msg attrs
