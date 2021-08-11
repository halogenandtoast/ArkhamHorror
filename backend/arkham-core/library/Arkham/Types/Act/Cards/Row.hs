module Arkham.Types.Act.Cards.Row
  ( Row(..)
  , row
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Enemy.Cards as Enemies
import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.Restriction
import qualified Arkham.Types.Restriction as R
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype Row = Row ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

row :: ActCard Row
row = act (3, A) Row Cards.row Nothing

instance HasActions Row where
  getActions (Row x) =
    [ mkAbility x 1 (ForcedAbility $ R.DrawCard Timing.When You IsEncounterCard)
    , restrictedAbility
      x
      2
      (LocationExists $ locationIs Cards.gondola <> LocationWithResources
        (GreaterThanOrEqualTo (PerPlayer 4))
      )
      (Objective $ ForcedAbility AnyWindow)
    ]

instance
  ( HasName env LocationId
  , HasCount ResourceCount env LocationId
  , HasId LocationId env InvestigatorId
  , ActRunner env
  )
  => RunMessage env Row where
  runMessage msg a@(Row attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      _ <- popMessage
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
