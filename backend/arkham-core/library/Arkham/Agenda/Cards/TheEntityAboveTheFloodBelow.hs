module Arkham.Agenda.Cards.TheEntityAboveTheFloodBelow (
  TheEntityAboveTheFloodBelow (..),
  theEntityAboveTheFloodBelow,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyTraits))
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait

newtype TheEntityAboveTheFloodBelow = TheEntityAboveTheFloodBelow AgendaAttrs
  deriving anyclass (IsAgenda)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

theEntityAboveTheFloodBelow :: AgendaCard TheEntityAboveTheFloodBelow
theEntityAboveTheFloodBelow =
  agenda
    (2, C)
    TheEntityAboveTheFloodBelow
    Cards.theEntityAboveTheFloodBelow
    (Static 6)

instance HasModifiersFor TheEntityAboveTheFloodBelow where
  getModifiersFor (EnemyTarget eid) (TheEntityAboveTheFloodBelow a) = do
    isMonster <- fieldP EnemyTraits (member Monster) eid
    pure $ toModifiers a [EnemyFight 1 | isMonster]
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
      lead <- getLead
      beast <- getSetAsideCard Enemies.beastOfAldebaran
      mChapel <- selectOne $ LocationWithTitle "Chapel of St. Aubert"
      spawnAshleighClarkeMessages <- do
        spawnAshleighClarke <- not <$> slain Enemies.ashleighClarke
        port <- selectJust $ LocationWithTitle "Porte de l’Avancée"
        card <- genCard Enemies.ashleighClarke
        createAshleighClarke <- createEnemyAt_ card port Nothing
        pure [createAshleighClarke | spawnAshleighClarke]

      createBeastOfAldebaran <- for (toList mChapel)
        $ \chapel -> createEnemyAt_ beast chapel Nothing

      pushAll
        $ createBeastOfAldebaran
        <> spawnAshleighClarkeMessages
        <> [ RemoveAllCopiesOfCardFromGame lead "03282"
           , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
           ]
      pure a
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      investigatorIds <- getInvestigatorIds
      pushAll
        $ [PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1, AdvanceAgendaIfThresholdSatisfied]
        <> [ TakeResources iid 2 (toAbilitySource attrs 1) False
           | iid <- investigatorIds
           ]
      pure a
    _ -> TheEntityAboveTheFloodBelow <$> runMessage msg attrs
