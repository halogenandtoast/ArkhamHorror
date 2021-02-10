module Arkham.Types.Enemy.Cards.ScreechingByakhee where

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
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner

newtype ScreechingByakhee = ScreechingByakhee EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

screechingByakhee :: EnemyId -> ScreechingByakhee
screechingByakhee uuid =
  ScreechingByakhee
    $ baseAttrs uuid "01175"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 2)
    . (fightL .~ 3)
    . (healthL .~ Static 4)
    . (evadeL .~ 3)
    . (preyL .~ LowestRemainingSanity)

instance HasCount RemainingSanity env InvestigatorId => HasModifiersFor env ScreechingByakhee where
  getModifiersFor _ target (ScreechingByakhee attrs) | isTarget attrs target =
    do
      sanities <- map unRemainingSanity
        <$> traverse getCount (setToList $ enemyEngagedInvestigators attrs)
      pure $ toModifiers attrs $ if any (<= 4) sanities
        then [EnemyFight 1, EnemyEvade 1]
        else []
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env ScreechingByakhee where
  getActions i window (ScreechingByakhee attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env ScreechingByakhee where
  runMessage msg (ScreechingByakhee attrs) =
    ScreechingByakhee <$> runMessage msg attrs
