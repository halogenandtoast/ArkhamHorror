module Arkham.Agenda.Cards.LetTheStormRageTheVortexAbove
  ( LetTheStormRageTheVortexAbove(..)
  , letTheStormRageTheVortexAbove
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
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Treachery.Cards qualified as Treacheries

newtype LetTheStormRageTheVortexAbove = LetTheStormRageTheVortexAbove AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

letTheStormRageTheVortexAbove :: AgendaCard LetTheStormRageTheVortexAbove
letTheStormRageTheVortexAbove = agenda
  (2, A)
  LetTheStormRageTheVortexAbove
  Cards.letTheStormRageTheVortexAbove
  (Static 6)

instance HasModifiersFor LetTheStormRageTheVortexAbove where
  getModifiersFor (TreacheryTarget tid) (LetTheStormRageTheVortexAbove a) = do
    isAncientEvils <- member tid
      <$> select (treacheryIs Treacheries.ancientEvils)
    pure $ toModifiers a [ AddKeyword Keyword.Surge | isAncientEvils ]
  getModifiersFor _ _ = pure []

instance HasAbilities LetTheStormRageTheVortexAbove where
  getAbilities (LetTheStormRageTheVortexAbove a) =
    [ limitedAbility (GroupLimit PerRound 1)
        $ mkAbility a 1
        $ FastAbility
        $ GroupClueCost (PerPlayer 1) Anywhere
    ]

instance RunMessage LetTheStormRageTheVortexAbove where
  runMessage msg a@(LetTheStormRageTheVortexAbove attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      beast <- getSetAsideCard Enemies.beastOfAldebaran
      abbeyTower <- maybeToList <$> selectOne (LocationWithTitle "Abbey Tower")
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
        $ [ CreateEnemyAt beast lid Nothing | lid <- abbeyTower ]
        <> spawnAshleighClarkeMessages
        <> [ RemoveAllCopiesOfCardFromGame leadInvestigatorId "03281"
           , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
           ]
      pure a
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      investigatorIds <- getInvestigatorIds
      pushAll
        $ [PlaceDoom (toTarget attrs) 1, AdvanceAgendaIfThresholdSatisfied]
        <> [ TakeResources iid 2 False | iid <- investigatorIds ]
      pure a
    _ -> LetTheStormRageTheVortexAbove <$> runMessage msg attrs
