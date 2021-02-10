module Arkham.Types.Enemy.Cards.AvianThrall
  ( AvianThrall(..)
  , avianThrall
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
import Arkham.Types.Game.Helpers
import Arkham.Types.Trait

newtype AvianThrall = AvianThrall EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

avianThrall :: EnemyId -> AvianThrall
avianThrall uuid =
  AvianThrall
    $ baseAttrs uuid "02094"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 1)
    . (fightL .~ 5)
    . (healthL .~ Static 4)
    . (evadeL .~ 3)
    . (preyL .~ LowestSkill SkillIntellect)

instance HasSet Trait env AssetId => HasModifiersFor env AvianThrall where
  getModifiersFor (AssetSource aid) target (AvianThrall attrs)
    | isTarget attrs target = do
      traits <- getSet aid
      pure $ toModifiers
        attrs
        [ EnemyFight (-3) | any (`elem` [Ranged, Firearm, Spell]) traits ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env AvianThrall where
  getActions i window (AvianThrall attrs) = getActions i window attrs

instance EnemyRunner env => RunMessage env AvianThrall where
  runMessage msg (AvianThrall attrs@EnemyAttrs {..}) =
    AvianThrall <$> runMessage msg attrs
