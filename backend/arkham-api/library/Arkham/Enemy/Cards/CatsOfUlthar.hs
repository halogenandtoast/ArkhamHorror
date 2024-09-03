module Arkham.Enemy.Cards.CatsOfUlthar (catsOfUlthar, CatsOfUlthar (..)) where

import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype CatsOfUlthar = CatsOfUlthar EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catsOfUlthar :: EnemyCard CatsOfUlthar
catsOfUlthar = enemy CatsOfUlthar Cards.catsOfUlthar (1, Static 1, 1) (1, 0)

-- This is a bit weird to handle, but this is the only card that does that so
-- we affect the first token drawn and treat it like you need to draw an
-- additional. It might be better in the future to move this as a modifier
-- directly on the skill test
instance HasModifiersFor CatsOfUlthar where
  getModifiersFor (SkillTestTarget _) (CatsOfUlthar a) = do
    modifiers <-
      toList <$> runMaybeT do
        st <- MaybeT getSkillTest
        guard $ a `is` st.target && st.action == Just #evade
        pure RevealAnotherChaosToken
    pure $ toModifiers a modifiers
  getModifiersFor _ _ = pure []

instance HasAbilities CatsOfUlthar where
  getAbilities (CatsOfUlthar x) =
    extend x
      $ [ restrict (notExists $ DefeatedEnemy $ enemyIs Cards.catsOfUlthar)
            $ forcedAbility x 1 (EnemyDefeated #when Anyone ByAny $ be x)
        ]

instance RunMessage CatsOfUlthar where
  runMessage msg e@(CatsOfUlthar attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      host <- selectJust $ enemyIs Cards.catsOfUlthar <> not_ IsSwarm
      lead <- getLead
      when (host == toId attrs)
        $ cancelEnemyDefeat (toId attrs)

      pushAll [PlaceSwarmCards lead host 3, RecordForInvestigator iid HasBrokenTheLawOfUlthar]
      pure e
    _ -> CatsOfUlthar <$> runMessage msg attrs
