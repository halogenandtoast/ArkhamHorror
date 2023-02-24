module Arkham.Treachery.Cards.SmiteTheWicked where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Id
import Arkham.Matcher
import Arkham.Message hiding ( InvestigatorEliminated )
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
    [ mkAbility a 1 $ ForcedAbility $ OrWindowMatcher
        [ GameEnds Timing.When
        , InvestigatorEliminated Timing.When (InvestigatorWithId iid)
        ]
    | iid <- maybeToList (treacheryOwner a)
    ]

instance RunMessage SmiteTheWicked where
  runMessage msg t@(SmiteTheWicked attrs@TreacheryAttrs {..}) = case msg of
    Revelation _iid source | isSource attrs source ->
      t <$ push
        (DiscardEncounterUntilFirst source Nothing (CardWithType EnemyType))
    RequestedEncounterCard source _ mcard | isSource attrs source ->
      case mcard of
        Nothing -> pure t
        Just card -> do
          let
            ownerId = fromJustNote "has to be set" treacheryOwner
            enemyId = EnemyId $ toCardId card
          farthestLocations <- selectList $ FarthestLocationFromYou Anywhere
          pushAll
            [ CreateEnemy (EncounterCard card)
            , AttachTreachery treacheryId (EnemyTarget enemyId)
            , chooseOne
              ownerId
              [ targetLabel lid [EnemySpawn Nothing lid enemyId]
              | lid <- farthestLocations
              ]
            ]
          pure t
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      let investigator = fromJustNote "missing investigator" treacheryOwner
      in t <$ push (SufferTrauma investigator 0 1)
    _ -> SmiteTheWicked <$> runMessage msg attrs
