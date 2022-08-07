module Arkham.Event.Cards.AnatomicalDiagrams
  ( anatomicalDiagrams
  , AnatomicalDiagrams(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Event.Runner
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype AnatomicalDiagrams = AnatomicalDiagrams EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anatomicalDiagrams :: EventCard AnatomicalDiagrams
anatomicalDiagrams = event AnatomicalDiagrams Cards.anatomicalDiagrams

instance RunMessage AnatomicalDiagrams where
  runMessage msg e@(AnatomicalDiagrams attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      enemies <-
        selectListMap EnemyTarget $ EnemyAt YourLocation <> NonEliteEnemy
      e <$ pushAll
        [ chooseOrRunOne
          iid
          [ TargetLabel
              enemy
              [ CreateWindowModifierEffect
                  EffectTurnWindow
                  (EffectModifiers
                  $ toModifiers attrs [EnemyFight (-2), EnemyEvade (-2)]
                  )
                  (toSource attrs)
                  enemy
              ]
          | enemy <- enemies
          ]
        , Discard (toTarget attrs)
        ]
    _ -> AnatomicalDiagrams <$> runMessage msg attrs
