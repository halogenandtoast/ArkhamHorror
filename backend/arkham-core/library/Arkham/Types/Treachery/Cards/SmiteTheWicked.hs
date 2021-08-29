module Arkham.Types.Treachery.Cards.SmiteTheWicked where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (InvestigatorEliminated)
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype SmiteTheWicked = SmiteTheWicked TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smiteTheWicked :: TreacheryCard SmiteTheWicked
smiteTheWicked = treachery SmiteTheWicked Cards.smiteTheWicked

instance HasAbilities env SmiteTheWicked where
  getAbilities _ _ (SmiteTheWicked a) = pure
    [ mkAbility a 1 $ ForcedAbility $ OrWindowMatcher
        [ GameEnds Timing.When
        , InvestigatorEliminated Timing.When (InvestigatorWithId iid)
        ]
    | iid <- maybeToList (treacheryOwner a)
    ]

instance TreacheryRunner env => RunMessage env SmiteTheWicked where
  runMessage msg t@(SmiteTheWicked attrs@TreacheryAttrs {..}) = case msg of
    Revelation _iid source | isSource attrs source ->
      t <$ push (DiscardEncounterUntilFirst source (CardWithType EnemyType))
    RequestedEncounterCard source mcard | isSource attrs source -> case mcard of
      Nothing -> t <$ push (Discard $ toTarget attrs)
      Just card -> do
        let
          ownerId = fromJustNote "has to be set" treacheryOwner
          enemyId = EnemyId $ toCardId card
        farthestLocations <- map unFarthestLocationId <$> getSetList ownerId
        t <$ pushAll
          [ CreateEnemy (EncounterCard card)
          , AttachTreachery treacheryId (EnemyTarget enemyId)
          , chooseOne
            ownerId
            [ EnemySpawn Nothing lid enemyId | lid <- farthestLocations ]
          ]
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      let investigator = fromJustNote "missing investigator" treacheryOwner
      in t <$ push (SufferTrauma investigator 0 1)
    _ -> SmiteTheWicked <$> runMessage msg attrs
