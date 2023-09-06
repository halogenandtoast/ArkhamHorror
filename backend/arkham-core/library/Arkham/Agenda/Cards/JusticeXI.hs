module Arkham.Agenda.Cards.JusticeXI (
  JusticeXI (..),
  justiceXI,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Types (Field (LocationClues))
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenarios.AtDeathsDoorstep.Story
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Monster, SilverTwilight))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype JusticeXI = JusticeXI AgendaAttrs
  deriving anyclass (IsAgenda)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

justiceXI :: AgendaCard JusticeXI
justiceXI = agenda (1, A) JusticeXI Cards.justiceXI (Static 8)

instance HasModifiersFor JusticeXI where
  getModifiersFor (EnemyTarget eid) (JusticeXI attrs) = do
    isSilverTwilight <- eid <=~> EnemyWithTrait SilverTwilight
    pure $ toModifiers attrs $ do
      guard isSilverTwilight
      [CannotBeDamaged, CannotBeDefeated]
  getModifiersFor _ _ = pure []

instance HasAbilities JusticeXI where
  getAbilities (JusticeXI a) =
    [ mkAbility a 1 $
        ForcedAbility $
          DrawCard
            Timing.When
            Anyone
            (BasicCardMatch $ CardWithTrait Monster)
            EncounterDeck
    ]

toDrawnCard :: [Window] -> Card
toDrawnCard [] = error "Missing DrawCard window"
toDrawnCard ((windowType -> Window.DrawCard _ card _) : _) = card
toDrawnCard (_ : xs) = toDrawnCard xs

instance RunMessage JusticeXI where
  runMessage msg a@(JusticeXI attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      iids <- getInvestigatorIds
      n <- getSpendableClueCount iids
      targetAmount <- perPlayer 3
      actId <- selectJust AnyAct
      allLocations <- selectList LocationWithAnyClues
      missingPersons <- getRecordedCardCodes MissingPersons
      noCluesOnEntryHall <-
        (== 0)
          <$> selectJustField LocationClues (LocationWithTitle "Entry Hall")
      noCluesOnOffice <-
        (== 0)
          <$> selectJustField LocationClues (LocationWithTitle "Office")
      noCluesOnBalcony <-
        (== 0)
          <$> selectJustField LocationClues (LocationWithTitle "Balcony")
      noCluesOnBilliardsRoom <-
        (== 0)
          <$> selectJustField LocationClues (LocationWithTitle "BillardsRoom")
      msgs <-
        if n < targetAmount
          then pure $ map (RemoveAllClues (toSource attrs) . toTarget) iids
          else do
            lead <- getLead
            lids <-
              selectList $
                FarthestLocationFromAll
                  (NotLocation $ LocationWithTitle "Entry Hall")
            josefMeiger <- getSetAsideCard Enemies.josefMeiger
            pure
              [ SpendClues targetAmount iids
              , chooseOne
                  lead
                  [targetLabel lid [SpawnEnemyAt josefMeiger lid] | lid <- lids]
              ]

      pushAll $
        msgs
          <> map (RemoveAllClues (toSource attrs) . toTarget) allLocations
          <> ( if toCardCode Investigators.gavriellaMizrah `elem` missingPersons && noCluesOnEntryHall
                then [story iids interlude1Gavriella, Record TheInvestigatorsAreOnGavriella'sTrail]
                else []
             )
          <> ( if toCardCode Investigators.jeromeDavids `elem` missingPersons && noCluesOnOffice
                then [story iids interlude1Jerome, Record TheInvestigatorsAreOnJerome'sTrail]
                else []
             )
          <> ( if toCardCode Investigators.pennyWhite `elem` missingPersons && noCluesOnBalcony
                then [story iids interlude1Penny, Record TheInvestigatorsAreOnPenny'sTrail]
                else []
             )
          <> ( if toCardCode Investigators.valentinoRivas `elem` missingPersons && noCluesOnBilliardsRoom
                then [story iids interlude1Valentino, Record TheInvestigatorsAreOnValentino'sTrail]
                else []
             )
          <> [ AdvanceAct actId (toSource attrs) AdvancedWithOther
             , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
             ]
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 (toDrawnCard -> card) _ -> do
      removeMessageType DrawEnemyMessage
      pushAll
        [ SetCardAside card
        , PlaceDoomOnAgenda
        , AdvanceAgendaIfThresholdSatisfied
        ]
      pure a
    _ -> JusticeXI <$> runMessage msg attrs
