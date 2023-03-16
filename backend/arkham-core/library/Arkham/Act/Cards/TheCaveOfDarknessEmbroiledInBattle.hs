module Arkham.Act.Cards.TheCaveOfDarknessEmbroiledInBattle
  ( TheCaveOfDarknessEmbroiledInBattle(..)
  , theCaveOfDarknessEmbroiledInBattle
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Name
import Arkham.ScenarioLogKey
import Arkham.Scenarios.ThreadsOfFate.Helpers
import Arkham.Trait

newtype TheCaveOfDarknessEmbroiledInBattle = TheCaveOfDarknessEmbroiledInBattle ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities TheCaveOfDarknessEmbroiledInBattle where
  getAbilities (TheCaveOfDarknessEmbroiledInBattle attrs) =
    [ restrictedAbility
          attrs
          999
          (LocationExists
          $ LocationWithTitle "Black Cave"
          <> LocationWithoutClues
          )
        $ Objective
        $ FastAbility
        $ GroupClueCost (PerPlayer 2)
        $ LocationWithTitle "Black Cave"
    ]

theCaveOfDarknessEmbroiledInBattle
  :: ActCard TheCaveOfDarknessEmbroiledInBattle
theCaveOfDarknessEmbroiledInBattle = act
  (2, E)
  TheCaveOfDarknessEmbroiledInBattle
  Cards.theCaveOfDarknessEmbroiledInBattle
  Nothing

instance RunMessage TheCaveOfDarknessEmbroiledInBattle where
  runMessage msg a@(TheCaveOfDarknessEmbroiledInBattle attrs) = case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide F attrs -> do
      deckCount <- getActDecksInPlayCount
      pushAll
        $ [ ShuffleEncounterDiscardBackIn
          , DiscardEncounterUntilFirst (toSource attrs) Nothing $ CardWithTrait Cultist
          ]
        <> [ DiscardEncounterUntilFirst (toSource attrs) Nothing $ CardWithTrait Cultist
           | deckCount <= 2
           ]
        <> [ AdvanceToAct
               (actDeckId attrs)
               Acts.theBrotherhoodIsRevealed
               E
               (toSource attrs)
           ]
      pure a
    RequestedEncounterCard source _ (Just ec) | isSource attrs source -> do
      blackCave <- selectJust $ locationIs Locations.blackCave
      (enemyId, enemyCreation) <- createEnemyAt (EncounterCard ec) blackCave Nothing
      pushAll
        [ enemyCreation
        , Remember $ IchtacasPrey $ labeled ec enemyId
        ]
      pure a
    _ -> TheCaveOfDarknessEmbroiledInBattle <$> runMessage msg attrs
