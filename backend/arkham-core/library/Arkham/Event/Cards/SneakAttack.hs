module Arkham.Event.Cards.SneakAttack where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher hiding (NonAttackDamageEffect)

newtype SneakAttack = SneakAttack EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

sneakAttack :: EventCard SneakAttack
sneakAttack = event SneakAttack Cards.sneakAttack

instance RunMessage SneakAttack where
  runMessage msg e@(SneakAttack attrs) = case msg of
    PlayThisEvent you eid | attrs `is` eid -> do
      enemies <- selectList $ ExhaustedEnemy <> enemiesColocatedWith you
      player <- getPlayer you
      push $ chooseOne player $ targetLabels enemies $ only . nonAttackEnemyDamage attrs 2
      pure e
    _ -> SneakAttack <$> runMessage msg attrs
