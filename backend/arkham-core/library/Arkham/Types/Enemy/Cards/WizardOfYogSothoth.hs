module Arkham.Types.Enemy.Cards.WizardOfYogSothoth
  ( WizardOfYogSothoth(..)
  , wizardOfYogSothoth
  )
where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Trait

newtype WizardOfYogSothoth = WizardOfYogSothoth EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wizardOfYogSothoth :: EnemyId -> WizardOfYogSothoth
wizardOfYogSothoth uuid =
  WizardOfYogSothoth
    $ baseAttrs uuid "02087"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 2)
    . (fightL .~ 4)
    . (healthL .~ Static 3)
    . (evadeL .~ 3)
    . (preyL .~ FewestCards)

instance HasModifiersFor env WizardOfYogSothoth where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env WizardOfYogSothoth where
  getActions i window (WizardOfYogSothoth attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env WizardOfYogSothoth where
  runMessage msg e@(WizardOfYogSothoth attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrewEncounterCard iid card
      | iid `elem` enemyEngagedInvestigators -> e <$ when
        (any (`member` ecTraits card) [Hex, Pact])
        (unshiftMessage (EnemyAttack iid enemyId))
    InvestigatorDrewPlayerCard iid card -> e <$ when
      (any (`member` pcTraits card) [Hex, Pact])
      (unshiftMessage (EnemyAttack iid enemyId))
    _ -> WizardOfYogSothoth <$> runMessage msg attrs
