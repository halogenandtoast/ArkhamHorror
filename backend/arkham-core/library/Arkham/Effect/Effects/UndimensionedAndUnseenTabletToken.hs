module Arkham.Effect.Effects.UndimensionedAndUnseenTabletToken (
  undimensionedAndUnseenTabletToken,
  UndimensionedAndUnseenTabletToken (..),
) where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Effect.Helpers
import Arkham.Effect.Runner
import Arkham.Enemy.Types (Field (..))
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers

newtype UndimensionedAndUnseenTabletToken = UndimensionedAndUnseenTabletToken EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

undimensionedAndUnseenTabletToken
  :: EffectArgs -> UndimensionedAndUnseenTabletToken
undimensionedAndUnseenTabletToken =
  UndimensionedAndUnseenTabletToken . uncurry4 (baseAttrs "02236")

instance HasModifiersFor UndimensionedAndUnseenTabletToken where
  getModifiersFor (ChaosTokenTarget (ChaosToken _ Tablet)) (UndimensionedAndUnseenTabletToken attrs) =
    do
      difficulty <- scenarioField ScenarioDifficulty
      pure
        [ toModifier
            attrs
            $ ChangeChaosTokenModifier
            $ if difficulty `elem` [Easy, Standard]
              then NegativeModifier 4
              else AutoFailModifier
        ]
  getModifiersFor _ _ = pure []

instance RunMessage UndimensionedAndUnseenTabletToken where
  runMessage msg e@(UndimensionedAndUnseenTabletToken attrs) = case msg of
    CreatedEffect eid _ _ (InvestigatorTarget iid) | eid == effectId attrs -> do
      broodOfYogSothoth <- getBroodOfYogSothoth
      broodOfYogSothothWithClues <-
        filterM
          (fieldMap EnemyClues (> 0))
          broodOfYogSothoth
      difficulty <- scenarioField ScenarioDifficulty
      let
        result =
          if difficulty `elem` [Easy, Standard]
            then "token is -4"
            else "automatically fail"
      player <- getPlayer iid
      unless (null broodOfYogSothothWithClues) $ do
        push
          $ chooseOne player
          $ Label
            ("Do not remove clues from Brood of Yog-Sothoth and " <> result)
            []
          : [ targetLabel
              enemyId
              [ RemoveAllClues (ChaosTokenEffectSource Tablet) (toTarget enemyId)
              , DisableEffect $ effectId attrs
              ]
            | enemyId <- broodOfYogSothothWithClues
            ]
      pure e
    SkillTestEnds _ _ -> e <$ push (DisableEffect $ effectId attrs)
    _ -> UndimensionedAndUnseenTabletToken <$> runMessage msg attrs
