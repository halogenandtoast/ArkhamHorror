module Arkham.Story.Cards.YigsMercy (
  YigsMercy (..),
  yigsMercy,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence
import Arkham.CampaignLogKey
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner
import Arkham.Trait qualified as Trait

newtype YigsMercy = YigsMercy StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

yigsMercy :: StoryCard YigsMercy
yigsMercy = story YigsMercy Cards.yigsMercy

instance RunMessage YigsMercy where
  runMessage msg s@(YigsMercy attrs) = case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      ichtaca <- selectJust $ enemyIs Enemies.ichtacaScionOfYig
      yigsFury <- getRecordCount YigsFury
      player <- getPlayer iid
      push
        $ chooseOne player
        $ if yigsFury >= 16
          then [Label "Ichtaca refuses your plea" []]
          else
            [ Label
                "I could never turn my back on humanity"
                [ Exhaust (toTarget ichtaca)
                , DisengageEnemyFromAll ichtaca
                , CreateWindowModifierEffect
                    EffectGameWindow
                    ( EffectModifiers
                        $ toModifiers
                          attrs
                          [CannotParleyWith $ enemyIs Enemies.ichtacaScionOfYig]
                    )
                    (toSource attrs)
                    (InvestigatorTarget iid)
                ]
            , Label
                "I accept"
                [ RemoveEnemy ichtaca
                , AdvanceToAct 1 Acts.timelock A (toSource attrs)
                , CreateWindowModifierEffect
                    EffectGameWindow
                    ( EffectModifiers
                        $ toModifiers
                          attrs
                          [ CannotParleyWith $ enemyIs Enemies.alejandroVela
                          , CannotBeAttackedBy $ EnemyWithTrait Trait.Cultist
                          , CannotBeEngagedBy $ EnemyWithTrait Trait.Cultist
                          ]
                    )
                    (toSource attrs)
                    (InvestigatorTarget iid)
                ]
            ]
      pure s
    _ -> YigsMercy <$> runMessage msg attrs
