module Arkham.Treachery.Cards.SmiteTheWicked where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message hiding (InvestigatorEliminated)
import qualified Arkham.Timing as Timing
import qualified Arkham.Treachery.Cards as Cards
import Arkham.Treachery.Runner

newtype SmiteTheWicked = SmiteTheWicked TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smiteTheWicked :: TreacheryCard SmiteTheWicked
smiteTheWicked = treachery SmiteTheWicked Cards.smiteTheWicked

instance HasAbilities SmiteTheWicked where
  getAbilities (SmiteTheWicked a) =
    [ mkAbility a 1 $ ForcedAbility $ OrWindowMatcher
        [ GameEnds Timing.When
        , InvestigatorEliminated Timing.When (InvestigatorWithId iid)
        ]
    | iid <- maybeToList (treacheryOwner a)
    ]

instance RunMessage SmiteTheWicked where
  runMessage msg t@(SmiteTheWicked attrs@TreacheryAttrs {..}) = case msg of
    Revelation _iid source | isSource attrs source -> do
      push $ DiscardEncounterUntilFirst source Nothing (CardWithType EnemyType)
      pure t
    RequestedEncounterCard source _ mcard | isSource attrs source -> do
      for_ mcard $ \card -> do
        let ownerId = fromJustNote "has to be set" treacheryOwner
        farthestLocations <- selectList $ FarthestLocationFromYou Anywhere
        (enemyId, enemyCreation) <- createEnemy (EncounterCard card)
        pushAll
          [ enemyCreation
          , AttachTreachery treacheryId (EnemyTarget enemyId)
          , chooseOne
            ownerId
            [ targetLabel lid [EnemySpawn Nothing lid enemyId]
            | lid <- farthestLocations
            ]
          ]
      pure t
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      let investigator = fromJustNote "missing investigator" treacheryOwner
      push $ SufferTrauma investigator 0 1
      pure t
    _ -> SmiteTheWicked <$> runMessage msg attrs
