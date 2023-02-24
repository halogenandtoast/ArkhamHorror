module Arkham.Agenda.Cards.TheEntityAboveTheFloodBelow
  ( TheEntityAboveTheFloodBelow(..)
  , theEntityAboveTheFloodBelow
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Enemy.Types ( Field (EnemyTraits) )
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Trait

newtype TheEntityAboveTheFloodBelow = TheEntityAboveTheFloodBelow AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEntityAboveTheFloodBelow :: AgendaCard TheEntityAboveTheFloodBelow
theEntityAboveTheFloodBelow = agenda
  (2, C)
  TheEntityAboveTheFloodBelow
  Cards.theEntityAboveTheFloodBelow
  (Static 6)

instance HasModifiersFor TheEntityAboveTheFloodBelow where
  getModifiersFor (EnemyTarget eid) (TheEntityAboveTheFloodBelow a) = do
    isMonster <- fieldP EnemyTraits (member Monster) eid
    pure $ toModifiers a [ EnemyFight 1 | isMonster ]
  getModifiersFor _ _ = pure []

instance HasAbilities TheEntityAboveTheFloodBelow where
  getAbilities (TheEntityAboveTheFloodBelow a) =
    [ limitedAbility (GroupLimit PerRound 1)
        $ mkAbility a 1
        $ FastAbility
        $ GroupClueCost (PerPlayer 1) Anywhere
    ]

instance RunMessage TheEntityAboveTheFloodBelow where
  runMessage msg a@(TheEntityAboveTheFloodBelow attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide D attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      beast <- getSetAsideCard Enemies.beastOfAldebaran
      chapel <- maybeToList <$> selectOne (LocationWithTitle "Chapel of St. Aubert")
      spawnAshleighClarkeMessages <- do
        spawnAshleighClarke <-
          notElem (Recorded $ toCardCode Enemies.ashleighClarke)
            <$> getRecordSet VIPsSlain
        if spawnAshleighClarke
          then do
            port <- selectJust $ LocationWithTitle "Porte de l’Avancée"
            card <- genCard Enemies.ashleighClarke
            pure [CreateEnemyAtLocationMatching card (LocationWithId port)]
          else pure []
      pushAll
        $ [CreateEnemyAt beast lid Nothing | lid <- chapel]
        <> spawnAshleighClarkeMessages
        <> [ RemoveAllCopiesOfCardFromGame leadInvestigatorId "03282"
           , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
           ]
      pure a
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      investigatorIds <- getInvestigatorIds
      pushAll
        $ [PlaceDoom (toTarget attrs) 1, AdvanceAgendaIfThresholdSatisfied]
        <> [ TakeResources iid 2 (toAbilitySource attrs 1) False | iid <- investigatorIds ]
      pure a
    _ -> TheEntityAboveTheFloodBelow <$> runMessage msg attrs
