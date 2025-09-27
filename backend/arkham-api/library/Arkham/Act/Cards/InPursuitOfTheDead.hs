module Arkham.Act.Cards.InPursuitOfTheDead (inPursuitOfTheDead) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheCircleUndone.Memento.Helpers
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getInvestigators, getJustLocationByName, getSetAsideCardsMatching)
import Arkham.Matcher
import Arkham.I18n
import Arkham.Scenarios.TheWagesOfSin.Helpers
import Arkham.Trait (Trait (Spectral))

newtype InPursuitOfTheDead = InPursuitOfTheDead ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- Errata: The text on this card should read: "Locations cannot be flipped to their spectral side"
instance HasModifiersFor InPursuitOfTheDead where
  getModifiersFor (InPursuitOfTheDead attrs) = do
    modifySelect attrs (not_ $ LocationWithTrait Spectral) [CannotBeFlipped]

inPursuitOfTheDead :: ActCard InPursuitOfTheDead
inPursuitOfTheDead = act (1, A) InPursuitOfTheDead Cards.inPursuitOfTheDead (groupClueCost (PerPlayer 3))

instance RunMessage InPursuitOfTheDead where
  runMessage msg a@(InPursuitOfTheDead attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      getSetAsideCardsMatching (CardWithTitle "Heretic") >>= \case
        [heretic1, heretic2, heretic3, heretic4] -> do
          theGallows <- getJustLocationByName "The Gallows"
          hereticsGraves <- getJustLocationByName "Heretics' Graves"
          chapelAttic <- getJustLocationByName "Chapel Attic"
          chapelCrypt <- getJustLocationByName "Chapel Crypt"

          createEnemyAt_ heretic1 theGallows
          createEnemyAt_ heretic2 hereticsGraves
          createEnemyAt_ heretic3 chapelAttic
          createEnemyAt_ heretic4 chapelCrypt

          let hereticLocations = [theGallows, hereticsGraves, chapelAttic, chapelCrypt]
          for_ hereticLocations \lid -> placeClues attrs lid =<< perPlayer 2
          selectEach (locationNotOneOf hereticLocations) \lid -> placeClues attrs lid =<< perPlayer 1

          mementosDiscovered <- getMementosDiscoveredCount
          when (mementosDiscovered >= 3) do
            send $ scenarioI18n $ ikey' "messages.understandTheTragicLyrics"
            investigators <- getInvestigators
            spectralWebs <- getSetAsideCardsMatching $ cardIs Assets.spectralWeb
            zipWithM_ takeControlOfSetAsideAsset investigators spectralWebs

          advanceActDeck attrs
        _ -> error "Invalid number of heretics"
      pure a
    _ -> InPursuitOfTheDead <$> liftRunMessage msg attrs
