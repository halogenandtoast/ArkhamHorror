module Arkham.Homebrew.DarkMatter.Agendas.ScreamOfTheDead (screamOfTheDead) where

import Arkham.Act.Sequence qualified as Act
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Window (assetLeavingPlay)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.Helpers (
  getEvidenceDeck,
  getScanningDeck,
  scanTopOfScanningDeck,
  scenarioI18n,
 )
import Arkham.Homebrew.DarkMatter.MotionScanning
import Arkham.Homebrew.DarkMatter.ScenarioDeckKeys (pattern EvidenceDeck, pattern ScanningDeck)

newtype ScreamOfTheDead = ScreamOfTheDead AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

screamOfTheDead :: AgendaCard ScreamOfTheDead
screamOfTheDead = agenda (3, A) ScreamOfTheDead Cards.screamOfTheDead (Static 3)

instance HasModifiersFor ScreamOfTheDead where
  getModifiersFor (ScreamOfTheDead a) = motionScanModifiers a

instance HasAbilities ScreamOfTheDead where
  getAbilities (ScreamOfTheDead a) = motionScanAbilities a

instance RunMessage ScreamOfTheDead where
  runMessage msg a@(ScreamOfTheDead attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      scanTopOfScanningDeck iid (attrs.ability 1)
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 (assetLeavingPlay -> aid) _ -> do
      push $ RemoveFromGame (AssetTarget aid)
      pure a
    {- Agenda 3b, "Hunger". Three mutually exclusive branches, and only the last
    one lets the agenda deck move on:

    "If there are still cards remaining in the scanning deck: Shuffle the
    scanning deck and remove the top card of the scanning deck from the game. If
    it is The Feaster from Afar, spawn it at the lead investigator's location
    instead. Flip this agenda back to agenda 3a.
    If the scanning deck is empty but not the 'Evidence' deck: Put the top card
    of the 'Evidence' deck under the scenario reference card. Flip this agenda
    back to agenda 3a.
    If both the scanning deck and the 'Evidence' deck are empty: The Entity
    reveals itself! Get ready for a fight! Advance to act 2b and agenda 4a."

    The Feaster is set aside at setup, so it can only be in the scanning deck
    because Call of the Void shuffled it in. -}
    AdvanceAgenda (isSide B attrs -> True) -> do
      scanning <- getScanningDeck
      evidence <- getEvidenceDeck
      scenarioI18n "inTheShadowOfEarth" $ scope "agenda3b" do
        if
          | notNull scanning -> do
              flavor $ setTitle "title" >> p "hunger"
              shuffle scanning >>= \case
                [] -> pure ()
                top : rest -> do
                  setScenarioDeck ScanningDeck rest
                  if toCardCode top == toCardCode Enemies.theFeasterFromAfar
                    then do
                      lead <- getLead
                      withLocationOf lead $ void . createEnemyAt top
                    else push $ RemovedFromGame top
              revertAgenda attrs
          | notNull evidence -> do
              case evidence of
                [] -> pure ()
                top : rest -> do
                  setScenarioDeck EvidenceDeck rest
                  -- Face down, like the copies setup hides there: act 2b and
                  -- resolution 1 both say to look at these without reading them.
                  placeUnderneath ScenarioTarget . (: []) =<< setFacedown True top
              revertAgenda attrs
          | otherwise -> do
              flavor $ setTitle "title" >> p "theEntityRevealsItself"
              -- Act 2b ("Quarantine") is what spawns The Entity and attaches
              -- the lost crew to it; it is reachable from here and nowhere else.
              advanceToAct' attrs 1 Acts.saveOurSouls Act.B
              advanceAgendaDeck attrs
      pure a
    _ -> ScreamOfTheDead <$> liftRunMessage msg attrs
