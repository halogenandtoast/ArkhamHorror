module Arkham.Agenda.Cards.LetTheStormRageTheVortexAbove (
  LetTheStormRageTheVortexAbove (..),
  letTheStormRageTheVortexAbove,
) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Treacheries

newtype LetTheStormRageTheVortexAbove = LetTheStormRageTheVortexAbove AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

letTheStormRageTheVortexAbove :: AgendaCard LetTheStormRageTheVortexAbove
letTheStormRageTheVortexAbove =
  agenda
    (2, A)
    LetTheStormRageTheVortexAbove
    Cards.letTheStormRageTheVortexAbove
    (Static 6)

instance HasModifiersFor LetTheStormRageTheVortexAbove where
  getModifiersFor (LetTheStormRageTheVortexAbove a) = do
    ancientEvils <- findAllCards (`isCard` Treacheries.ancientEvils)
    modifyEach a ancientEvils [AddKeyword Keyword.Surge]

instance HasAbilities LetTheStormRageTheVortexAbove where
  getAbilities (LetTheStormRageTheVortexAbove a) =
    [groupLimit PerRound $ mkAbility a 1 $ FastAbility $ GroupClueCost (PerPlayer 1) Anywhere]

instance RunMessage LetTheStormRageTheVortexAbove where
  runMessage msg a@(LetTheStormRageTheVortexAbove attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      lead <- getLead
      beast <- getSetAsideCard Enemies.beastOfAldebaran
      mAbbeyTower <- selectOne $ LocationWithTitle "Abbey Tower"
      spawnAshleighClarkeMessages <- do
        spawnAshleighClarke <- not <$> slain Enemies.ashleighClarke
        port <- selectJust $ locationIs $ Locations.porteDeLAvancee
        card <- genCard Enemies.ashleighClarke
        createAshleighClarke <- createEnemyAt_ card port Nothing
        pure [createAshleighClarke | spawnAshleighClarke]

      createBeastOfAldebaran <- for (toList mAbbeyTower)
        $ \abbeyTower -> createEnemyAt_ beast abbeyTower Nothing

      pushAll
        $ createBeastOfAldebaran
        <> spawnAshleighClarkeMessages
        <> [ RemoveAllCopiesOfCardFromGame lead "03281"
           , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
           ]
      pure a
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      investigatorIds <- getInvestigators
      pushAll
        $ [PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1, AdvanceAgendaIfThresholdSatisfied]
        <> [ TakeResources iid 2 (toAbilitySource attrs 1) False
           | iid <- investigatorIds
           ]
      pure a
    _ -> LetTheStormRageTheVortexAbove <$> runMessage msg attrs
