module Arkham.Homebrew.DarkMatter.Agendas.TheNostalgiaII (theNostalgiaII) where

import Arkham.Agenda.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Window (assetLeavingPlay)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Helpers (scanTopOfScanningDeck, scenarioI18n)
import Arkham.Homebrew.DarkMatter.MotionScanning
import Arkham.Matcher

newtype TheNostalgiaII = TheNostalgiaII AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNostalgiaII :: AgendaCard TheNostalgiaII
theNostalgiaII = agenda (1, A) TheNostalgiaII Cards.theNostalgiaII (Static 3)

instance HasModifiersFor TheNostalgiaII where
  getModifiersFor (TheNostalgiaII a) = motionScanModifiers a

instance HasAbilities TheNostalgiaII where
  getAbilities (TheNostalgiaII a) = motionScanAbilities a

instance RunMessage TheNostalgiaII where
  runMessage msg a@(TheNostalgiaII attrs) = runQueueT $ case msg of
    {- The clue cost has already been paid as a group by the time we get here,
    and the restriction guarantees the top card matches, so the scan just draws
    it. 'scanTopOfScanningDeck' also removes the card from the scanning deck and
    fires the scan windows that act 1 ("when you draw a story asset from the
    scanning deck, advance") and the Evidence cards listen for. -}
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      scanTopOfScanningDeck iid (attrs.ability 1)
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 (assetLeavingPlay -> aid) _ -> do
      push $ RemoveFromGame (AssetTarget aid)
      pure a
    {- Agenda 1b, "In the Open":

    "Shuffle the encounter discard pile into the encounter deck.
    Discard cards from the top of the encounter deck until an enemy is
    discarded. Spawn that enemy at the Ship Mainframe." -}
    AdvanceAgenda (isSide B attrs -> True) -> do
      scenarioI18n "inTheShadowOfEarth" $ scope "agenda1b" do
        flavor $ setTitle "title" >> p "body"
      shuffleEncounterDiscardBackIn
      lead <- getLead
      discardUntilFirst lead attrs Deck.EncounterDeck #enemy
      advanceAgendaDeck attrs
      pure a
    RequestedEncounterCard (isSource attrs -> True) _ (Just ec) -> do
      mainframe <- selectJust $ locationIs Locations.shipMainframe
      void $ createEnemyAt ec mainframe
      pure a
    _ -> TheNostalgiaII <$> liftRunMessage msg attrs
