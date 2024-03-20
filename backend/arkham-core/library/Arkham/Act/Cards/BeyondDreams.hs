module Arkham.Act.Cards.BeyondDreams (BeyondDreams (..), beyondDreams) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers (perPlayer)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries

newtype BeyondDreams = BeyondDreams ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beyondDreams :: ActCard BeyondDreams
beyondDreams = act (3, A) BeyondDreams Cards.beyondDreams Nothing

instance HasAbilities BeyondDreams where
  getAbilities (BeyondDreams x) =
    [ restrictedAbility
        x
        1
        ( EachUndefeatedInvestigator
            $ at_
            $ locationIs Locations.onyxGates
            <> LocationWithoutClues
        )
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage BeyondDreams where
  runMessage msg a@(BeyondDreams attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      eachInvestigator \iid -> do
        push $ InvestigatorDiscardAllClues (toSource attrs) iid

      story $ i18n "dreamEaters.whereTheGodsDwell.grandDesign1"

      theOnyxCastle <- selectJust $ locationIs Locations.theOnyxCastle
      reveal theOnyxCastle

      moveAllTo attrs theOnyxCastle

      selectEach (not_ $ locationIs Locations.theOnyxCastle) (push . RemoveLocation)

      survivedTheVoyage <- getHasRecord RandolphSurvivedTheVoyage

      if survivedTheVoyage
        then do
          story $ i18n "dreamEaters.whereTheGodsDwell.grandDesign2"
          removeCampaignCard Assets.randolphCarterExpertDreamer
        else do
          story $ i18n "dreamEaters.whereTheGodsDwell.grandDesign3"

      atYourSide <- getHasRecord TheBlackCatIsAtYourSide
      when atYourSide do
        story $ i18n "dreamEaters.whereTheGodsDwell.grandDesign4"

      story $ i18n "dreamEaters.whereTheGodsDwell.grandDesign5"

      n <- perPlayer 1

      (nyarlathoteps, removedNyarlathoteps) :: ([Card], [Card]) <-
        fmap (splitAt (n + 1)) . shuffleM =<< getSetAsideCardsMatching (CardWithTitle "Nyarlathotep")
      otherCards <-
        getSetAsideCardsMatching
          $ oneOf
            [ cardIs Enemies.theCrawlingMist
            , cardIs Treacheries.myriadForms
            , CardWithTitle "Whispering Chaos"
            ]

      push $ ShuffleCardsIntoDeck Deck.EncounterDeck (nyarlathoteps <> otherCards)
      shuffleEncounterDiscardBackIn

      (towers, removedTowers) :: ([Card], [Card]) <-
        fmap (splitAt 4) . shuffleM =<< getSetAsideCardsMatching (CardWithTitle "Forsaken Tower")

      for_ removedNyarlathoteps $ push . RemoveAllCopiesOfEncounterCardFromGame . CardWithId . toCardId
      for_ removedTowers $ push . RemoveAllCopiesOfEncounterCardFromGame . CardWithId . toCardId

      for_ (zip ["northTower", "southTower", "eastTower", "westTower"] towers)
        $ \(name, card) -> do
          tower <- placeLocation card
          push $ SetLocationLabel tower name

      push
        $ SetLayout
          [ ".         northTower    ."
          , "westTower theOnyxCastle eastTower"
          , ".         southTower    ."
          ]

      advanceActDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      advanceVia #other attrs iid
      pure a
    _ -> BeyondDreams <$> lift (runMessage msg attrs)
