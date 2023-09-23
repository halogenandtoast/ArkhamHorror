module Arkham.Treachery.Cards.SmiteTheWicked where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Creation
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Message hiding (InvestigatorEliminated)
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SmiteTheWicked = SmiteTheWicked TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smiteTheWicked :: TreacheryCard SmiteTheWicked
smiteTheWicked = treachery SmiteTheWicked Cards.smiteTheWicked

instance HasAbilities SmiteTheWicked where
  getAbilities (SmiteTheWicked a) =
    [ mkAbility a 1
      $ ForcedAbility
      $ OrWindowMatcher
        [ GameEnds Timing.When
        , InvestigatorEliminated Timing.When (InvestigatorWithId iid)
        ]
    | iid <- maybeToList (treacheryOwner a)
    ]

instance RunMessage SmiteTheWicked where
  runMessage msg t@(SmiteTheWicked attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      key <- getEncounterDeckKey iid
      push
        $ DiscardUntilFirst iid source (Deck.EncounterDeckByKey key) (BasicCardMatch $ CardWithType EnemyType)
      pure t
    RequestedEncounterCard source _ mcard | isSource attrs source -> do
      for_ mcard $ \card -> do
        let ownerId = fromJustNote "has to be set" treacheryOwner
        enemyCreation <- createEnemy (EncounterCard card) (FarthestLocationFromYou Anywhere)
        pushAll
          [ toMessage $ enemyCreation {enemyCreationInvestigator = Just ownerId}
          , AttachTreachery treacheryId (toTarget $ enemyCreationEnemyId enemyCreation)
          ]
      pure t
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      let investigator = fromJustNote "missing investigator" treacheryOwner
      push $ SufferTrauma investigator 0 1
      pure t
    _ -> SmiteTheWicked <$> runMessage msg attrs
