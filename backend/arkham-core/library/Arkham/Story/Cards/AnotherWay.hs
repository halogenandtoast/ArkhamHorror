module Arkham.Story.Cards.AnotherWay (
  AnotherWay (..),
  anotherWay,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner
import Arkham.Trait qualified as Trait

newtype AnotherWay = AnotherWay StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anotherWay :: StoryCard AnotherWay
anotherWay = story AnotherWay Cards.anotherWay

instance RunMessage AnotherWay where
  runMessage msg s@(AnotherWay attrs) = case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      alejandro <- selectJust $ enemyIs Enemies.alejandroVela
      player <- getPlayer iid
      push
        $ chooseOne player
        $ [ Label
              "I could never turn my back on humanity"
              [ Exhaust (toTarget alejandro)
              , DisengageEnemyFromAll alejandro
              , CreateWindowModifierEffect
                  EffectGameWindow
                  ( EffectModifiers
                      $ toModifiers
                        attrs
                        [CannotParleyWith $ enemyIs Enemies.alejandroVela]
                  )
                  (toSource attrs)
                  (InvestigatorTarget iid)
              ]
          , Label
              "I accept"
              [ RemoveEnemy alejandro
              , AdvanceToAct 1 Acts.timelock A (toSource attrs)
              , CreateWindowModifierEffect
                  EffectGameWindow
                  ( EffectModifiers
                      $ toModifiers
                        attrs
                        [ CannotParleyWith $ enemyIs Enemies.ichtacaScionOfYig
                        , CannotBeAttackedBy $ EnemyWithTrait Trait.Cultist
                        , CannotBeEngagedBy $ EnemyWithTrait Trait.Cultist
                        ]
                  )
                  (toSource attrs)
                  (InvestigatorTarget iid)
              ]
          ]
      pure s
    _ -> AnotherWay <$> runMessage msg attrs
