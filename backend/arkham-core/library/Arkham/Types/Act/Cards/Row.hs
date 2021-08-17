module Arkham.Types.Act.Cards.Row
  ( Row(..)
  , row
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Enemy.Cards as Enemies
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

newtype Row = Row ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

row :: ActCard Row
row = act (3, A) Row Cards.row Nothing

instance ActionRunner env => HasAbilities env Row where
  getAbilities iid (Window Timing.When (WouldDrawEncounterCard who)) (Row x)
    | iid == who = pure [mkAbility x 1 LegacyForcedAbility]
  getAbilities iid window (Row x) = getAbilities iid window x

instance
  ( HasName env LocationId
  , HasCount ResourceCount env LocationId
  , HasId LocationId env InvestigatorId
  , ActRunner env
  )
  => RunMessage env Row where
  runMessage msg a@(Row attrs) = case msg of
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 1)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      _ <- popMessage
      a <$ push (DiscardTopOfEncounterDeck iid 5 (Just $ toTarget attrs))
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
    PlaceResources (LocationTarget lid) n -> do
      locationName <- getName lid
      a <$ when
        (nameTitle locationName == "Gondola")
        do
          resources <- unResourceCount <$> getCount lid
          when
            (resources + n >= 4)
            (push $ AdvanceAct (toId attrs) (toSource attrs))
    _ -> Row <$> runMessage msg attrs
