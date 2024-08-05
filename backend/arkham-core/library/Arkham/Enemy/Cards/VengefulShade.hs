module Arkham.Enemy.Cards.VengefulShade (vengefulShade, VengefulShade (..)) where

import Arkham.Attack
import Arkham.Classes
import Arkham.Constants
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude

newtype VengefulShade = VengefulShade EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vengefulShade :: EnemyCard VengefulShade
vengefulShade =
  enemyWith VengefulShade Cards.vengefulShade (5, Static 2, 5) (0, 2)
    $ (spawnAtL ?~ NoSpawn)
    . ( preyL
          .~ OnlyPrey
            (oneOf [investigatorIs Investigators.jimCulver, investigatorIs Investigators.jimCulverParallel])
      )

instance HasAbilities VengefulShade where
  getAbilities (VengefulShade a) = case enemyPlacement a of
    AttachedToAsset _ _ ->
      case enemyBearer a of
        Nothing -> error "No bearer"
        Just iid ->
          [ restrictedAbility
              a
              AbilityAttack
              ( exists (You <> InvestigatorWithId iid)
                  <> EnemyCriteria (ThisEnemy $ EnemyWithoutModifier CannotBeAttacked)
                  <> CanAttack
              )
              $ ActionAbility [#fight] (ActionCost 1)
          , restrictedAbility
              a
              AbilityEvade
              (exists (You <> InvestigatorWithId iid))
              $ ActionAbility [#evade] (ActionCost 1)
          , mkAbility a 1 $ forced NotAnyWindow -- the beyond will call this
          ]
    _ -> extend a [mkAbility a 1 $ forced NotAnyWindow] -- the beyond will call this

instance RunMessage VengefulShade where
  runMessage msg e@(VengefulShade attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ InitiateEnemyAttack $ enemyAttack (toId attrs) attrs iid
      pure e
    Successful (action, _) iid _ (isTarget attrs -> True) _ | action `elem` [#fight, #evade] -> do
      case enemyPlacement attrs of
        AttachedToAsset _ _ -> do
          push $ toDiscardBy iid (toSource attrs) attrs
          pure e
        _ -> VengefulShade <$> liftRunMessage msg attrs
    FailedSkillTest iid (Just action) _ (Initiator (isTarget attrs -> True)) _ _ | action `elem` [#fight, #evade] -> do
      case enemyPlacement attrs of
        AttachedToAsset {} -> do
          push $ EnemySpawnAtLocationMatching (Just iid) (locationWithInvestigator iid) (toId attrs)
          pure e
        _ -> VengefulShade <$> liftRunMessage msg attrs
    _ -> VengefulShade <$> liftRunMessage msg attrs
