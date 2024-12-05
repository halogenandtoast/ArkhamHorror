module Arkham.Enemy.Cards.TheCrawlingMist (
  theCrawlingMist,
  TheCrawlingMist (..),
)
where

import Arkham.Prelude

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
  getModifiersFor (TheCrawlingMist a) = do
    getSkillTest >>= \case
      Nothing -> pure mempty
      Just st -> maybeModified_ a (SkillTestTarget st.id) do
        eid <- hoistMaybe st.target.enemy
        guard $ eid == a.id
        liftGuardM $ orM [isFighting a, isEvading a]
        n <- lift $ selectCount $ TreacheryInHandOf (InvestigatorWithId st.investigator)
        m <- lift $ selectCount $ EnemyInHandOf (InvestigatorWithId st.investigator) <> NonWeaknessEnemy
        pure [Difficulty $ n + m]

instance RunMessage TheCrawlingMist where
  runMessage msg (TheCrawlingMist attrs) =
    TheCrawlingMist <$> runMessage msg attrs
