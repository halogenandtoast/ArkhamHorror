module Arkham.Types.Event.Cards.AnatomicalDiagrams
  ( anatomicalDiagrams
  , AnatomicalDiagrams(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype AnatomicalDiagrams = AnatomicalDiagrams EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anatomicalDiagrams :: EventCard AnatomicalDiagrams
anatomicalDiagrams = event AnatomicalDiagrams Cards.anatomicalDiagrams

instance EventRunner env => RunMessage env AnatomicalDiagrams where
  runMessage msg e@(AnatomicalDiagrams attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
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
