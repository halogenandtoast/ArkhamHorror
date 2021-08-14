module Arkham.Types.Enemy.Cards.WizardOfYogSothoth
  ( WizardOfYogSothoth(..)
  , wizardOfYogSothoth
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.Trait

newtype WizardOfYogSothoth = WizardOfYogSothoth EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wizardOfYogSothoth :: EnemyCard WizardOfYogSothoth
wizardOfYogSothoth = enemyWith
  WizardOfYogSothoth
  Cards.wizardOfYogSothoth
  (4, Static 3, 3)
  (1, 2)
  (preyL .~ FewestCards)

instance HasModifiersFor env WizardOfYogSothoth

instance ActionRunner env => HasAbilities env WizardOfYogSothoth where
  getAbilities i window (WizardOfYogSothoth attrs) = getAbilities i window attrs

instance (EnemyRunner env) => RunMessage env WizardOfYogSothoth where
  runMessage msg e@(WizardOfYogSothoth attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrewEncounterCard iid card
      | iid `elem` enemyEngagedInvestigators -> e <$ when
        (any (`member` toTraits card) [Hex, Pact])
        (push (EnemyAttack iid enemyId DamageAny))
    InvestigatorDrewPlayerCard iid card -> e <$ when
      (any (`member` toTraits card) [Hex, Pact])
      (push (EnemyAttack iid enemyId DamageAny))
    _ -> WizardOfYogSothoth <$> runMessage msg attrs
