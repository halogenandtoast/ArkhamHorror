module Arkham.Act.Cards.InPursuitOfAnswers (inPursuitOfAnswers) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getInvestigators, getJustLocationByName, getSetAsideCardsMatching)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Trait (Trait (Spectral))

newtype InPursuitOfAnswers = InPursuitOfAnswers ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

inPursuitOfAnswers :: ActCard InPursuitOfAnswers
inPursuitOfAnswers = act (1, A) InPursuitOfAnswers Cards.inPursuitOfAnswers (groupClueCost (PerPlayer 3))

instance HasModifiersFor InPursuitOfAnswers where
  getModifiersFor (InPursuitOfAnswers attrs) = do
    modifySelect attrs (not_ $ LocationWithTrait Spectral) [CannotBeFlipped]

instance RunMessage InPursuitOfAnswers where
  runMessage msg a@(InPursuitOfAnswers attrs) = runQueueT $ case msg of
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

          hauntedFields <- getJustLocationByName "Haunted Fields"
          erynn <- fetchCard $ SetAsideCard Assets.erynnMacAoidhDevotedEnchantress
          createAssetAt_ erynn (AtLocation hauntedFields)

          investigators <- getInvestigators
          spectralWebs <- getSetAsideCardsMatching $ cardIs Assets.spectralWeb
          zipWithM_ takeControlOfSetAsideAsset investigators spectralWebs

          setActDeckN 1 [Cards.inPursuitOfAnswers, Cards.inPursuitOfTheLiving]
          returnToAct <- genCard Cards.inPursuitOfTheBeyond
          push $ AddAct 2 returnToAct
          setDecksLayout
            [ ".       act1"
            , "agenda1 act1"
            , "agenda1 act2"
            , ".       act2"
            ]
          advanceActDeck attrs
        _ -> error "Invalid number of heretics"
      pure a
    _ -> InPursuitOfAnswers <$> liftRunMessage msg attrs
