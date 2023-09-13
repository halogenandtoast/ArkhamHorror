module Arkham.Enemy.Cards.PiperOfAzathoth (
  piperOfAzathoth,
  PiperOfAzathoth (..),
)
where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Timing qualified as Timing

newtype PiperOfAzathoth = PiperOfAzathoth EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

piperOfAzathoth :: EnemyCard PiperOfAzathoth
piperOfAzathoth = enemy PiperOfAzathoth Cards.piperOfAzathoth (5, Static 7, 2) (0, 2)

instance HasAbilities PiperOfAzathoth where
  getAbilities (PiperOfAzathoth a) = [mkAbility a 1 $ ForcedAbility $ PhaseEnds Timing.When $ PhaseIs EnemyPhase]

instance RunMessage PiperOfAzathoth where
  runMessage msg e@(PiperOfAzathoth attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      investigators <-
        selectList
          $ InvestigatorAt (locationWithEnemy $ toId e) <> NotInvestigator (investigatorEngagedWith $ toId e)
      pushAll
        [ toMessage $ enemyAttack (toId e) (toAbilitySource attrs 1) investigator
        | investigator <- investigators
        ]
      pure e
    _ -> PiperOfAzathoth <$> runMessage msg attrs
