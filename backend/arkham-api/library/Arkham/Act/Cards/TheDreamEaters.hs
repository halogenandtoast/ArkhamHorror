module Arkham.Act.Cards.TheDreamEaters (TheDreamEaters (..), theDreamEaters) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (HealthModifier), modifySelect)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher

newtype TheDreamEaters = TheDreamEaters ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDreamEaters :: ActCard TheDreamEaters
theDreamEaters = act (5, A) TheDreamEaters Cards.theDreamEaters Nothing

instance HasModifiersFor TheDreamEaters where
  getModifiersFor (TheDreamEaters attrs) = do
    clues <- selectSum InvestigatorClues UneliminatedInvestigator
    modifySelect attrs (enemyIs Enemies.nyarlathotepTrueShape) [HealthModifier (-clues) | clues > 0]

instance HasAbilities TheDreamEaters where
  getAbilities (TheDreamEaters x) =
    [ mkAbility x 1
        $ Objective
        $ forced
        $ EnemyDefeated #after Anyone ByAny
        $ enemyIs Enemies.nyarlathotepTrueShape
    ]

instance RunMessage TheDreamEaters where
  runMessage msg a@(TheDreamEaters attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      advanceVia #other attrs iid
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> TheDreamEaters <$> liftRunMessage msg attrs
