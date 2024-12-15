module Arkham.Agenda.Cards.JusticeXI (JusticeXI (..), justiceXI) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Log (getRecordedCardCodes)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getInvestigators, getLead, getSetAsideCard)
import Arkham.Helpers.Window
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Types (Field (LocationClues))
import Arkham.Matcher
import Arkham.Scenarios.AtDeathsDoorstep.Story
import Arkham.Trait (Trait (Monster, SilverTwilight))

newtype JusticeXI = JusticeXI AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

justiceXI :: AgendaCard JusticeXI
justiceXI = agenda (1, A) JusticeXI Cards.justiceXI (Static 8)

instance HasModifiersFor JusticeXI where
  getModifiersFor (JusticeXI a) =
    modifySelect a (EnemyWithTrait SilverTwilight) [CannotBeDamaged, CannotBeDefeated]

instance HasAbilities JusticeXI where
  getAbilities (JusticeXI a) =
    [mkAbility a 1 $ forced $ DrawCard #when Anyone (basic $ CardWithTrait Monster) EncounterDeck]

instance RunMessage JusticeXI where
  runMessage msg a@(JusticeXI attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      iids <- getInvestigators
      n <- getSpendableClueCount iids
      targetAmount <- perPlayer 3
      actId <- selectJust AnyAct
      allLocations <- select LocationWithAnyClues
      missingPersons <- getRecordedCardCodes MissingPersons
      let hasNoClues name = (== 0) <$> selectJustField LocationClues (LocationWithTitle name)
      noCluesOnEntryHall <- hasNoClues "Entry Hall"
      noCluesOnOffice <- hasNoClues "Office"
      noCluesOnBalcony <- hasNoClues "Balcony"
      noCluesOnBilliardsRoom <- hasNoClues "Billiards Room"
      if n < targetAmount
        then pushAll $ map (RemoveAllClues (toSource attrs) . toTarget) iids
        else do
          lead <- getLead
          lids <- select $ FarthestLocationFromAll (NotLocation "Entry Hall")
          josefMeiger <- getSetAsideCard Enemies.josefMeiger
          push $ SpendClues targetAmount iids
          chooseOne lead $ targetLabels lids (only . SpawnEnemyAt josefMeiger)
      pushAll $ map (RemoveAllClues (toSource attrs) . toTarget) allLocations
      when (Investigators.gavriellaMizrah.cardCode `elem` missingPersons && noCluesOnEntryHall) do
        story interlude1Gavriella
        record TheInvestigatorsAreOnGavriella'sTrail
      when (Investigators.jeromeDavids.cardCode `elem` missingPersons && noCluesOnOffice) do
        story interlude1Jerome
        record TheInvestigatorsAreOnJerome'sTrail
      when (Investigators.pennyWhite.cardCode `elem` missingPersons && noCluesOnBalcony) do
        story interlude1Penny
        record TheInvestigatorsAreOnPenny'sTrail
      when (Investigators.valentinoRivas.cardCode `elem` missingPersons && noCluesOnBilliardsRoom) do
        story interlude1Valentino
        record TheInvestigatorsAreOnValentino'sTrail
      push $ AdvanceAct actId (toSource attrs) #other
      advanceAgendaDeck attrs
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      quietCancelCardDraw card
      pushAll [UnfocusCards, SetCardAside card]
      placeDoomOnAgendaAndCheckAdvance 1
      pure a
    _ -> JusticeXI <$> liftRunMessage msg attrs
