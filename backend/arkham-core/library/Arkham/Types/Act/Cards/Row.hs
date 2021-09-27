module Arkham.Types.Act.Cards.Row
  ( Row(..)
  , row
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Resolution
import Arkham.Types.Timing qualified as Timing

newtype Row = Row ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

row :: ActCard Row
row = act (3, A) Row Cards.row Nothing

instance HasAbilities Row where
  getAbilities (Row x) =
    [ mkAbility x 1 $ ForcedAbility $ WouldDrawEncounterCard Timing.When You
    , restrictedAbility
      x
      2
      (ResourcesOnLocation (LocationWithTitle "Gondola") (AtLeast $ PerPlayer 4)
      )
    $ Objective
    $ ForcedAbility AnyWindow
    ]

instance
  ( HasId LocationId env InvestigatorId
  , ActRunner env
  )
  => RunMessage env Row where
  runMessage msg a@(Row attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      _ <- popMessageMatching $ \case
        InvestigatorDoDrawEncounterCard iid' -> iid == iid'
        _ -> False
      a <$ push (DiscardTopOfEncounterDeck iid 5 (Just $ toTarget attrs))
    UseCardAbility _ source _ 2 _ | isSource attrs source -> do
      a <$ push (AdvanceAct (toId attrs) source)
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 1)
    DiscardedTopOfEncounterDeck iid cards target | isTarget attrs target -> do
      lid <- getId @LocationId iid
      let
        writhingAppendages =
          filter ((== Enemies.writhingAppendage) . toCardDef) cards
      a <$ pushAll
        (concat
          [ [ RemoveFromEncounterDiscard card
            , SpawnEnemyAtEngagedWith (EncounterCard card) lid iid
            ]
          | card <- writhingAppendages
          ]
        )
    _ -> Row <$> runMessage msg attrs
