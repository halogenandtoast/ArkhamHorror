module Arkham.Enemy.Cards.TheCrawlingMist (
  theCrawlingMist,
  TheCrawlingMist (..),
)
where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype TheCrawlingMist = TheCrawlingMist EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theCrawlingMist :: EnemyCard TheCrawlingMist
theCrawlingMist =
  enemyWith
    TheCrawlingMist
    Cards.theCrawlingMist
    (3, Static 5, 3)
    (1, 1)
    (preyL .~ Prey MostCardsInHand)

instance HasModifiersFor TheCrawlingMist where
  getModifiersFor (SkillTestTarget _) (TheCrawlingMist a) = do
    mTarget <- getSkillTestTarget
    mAction <- getSkillTestAction
    mInvestigator <- getSkillTestInvestigator
    case (mTarget, mAction, mInvestigator) of
      (Just target, Just action, Just iid)
        | target == toTarget a
        , action `elem` [Action.Fight, Action.Evade] -> do
            n <- selectCount $ TreacheryInHandOf (InvestigatorWithId iid)
            m <- selectCount $ EnemyInHandOf (InvestigatorWithId iid) <> NonWeaknessEnemy
            pure $ toModifiers a [Difficulty $ n + m]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage TheCrawlingMist where
  runMessage msg (TheCrawlingMist attrs) =
    TheCrawlingMist <$> runMessage msg attrs
