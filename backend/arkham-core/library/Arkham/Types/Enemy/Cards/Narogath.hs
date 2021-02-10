module Arkham.Types.Enemy.Cards.Narogath where

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
 hiding (Cultist)

import Arkham.Types.Action
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Trait
import qualified Arkham.Types.Trait as Trait

newtype Narogath = Narogath EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

narogath :: EnemyId -> Narogath
narogath uuid =
  Narogath
    $ baseAttrs uuid "50026b"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 2)
    . (fightL .~ 3)
    . (healthL .~ Static 4)
    . (evadeL .~ 3)
    . (preyL .~ NearestToEnemyWithTrait Trait.Cultist)
    . (uniqueL .~ True)

instance (HasSet InvestigatorId env LocationId, HasSet ConnectedLocationId env LocationId) => HasModifiersFor env Narogath where
  getModifiersFor _ (InvestigatorTarget iid) (Narogath a@EnemyAttrs {..})
    | spawned a = do
      connectedLocationIds <- map unConnectedLocationId
        <$> getSetList enemyLocation
      iids <- concat <$> for (enemyLocation : connectedLocationIds) getSetList
      pure $ toModifiers
        a
        [ CannotTakeAction (EnemyAction Parley [Cultist])
        | not enemyExhausted && iid `elem` iids
        ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env Narogath where
  getActions i window (Narogath attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env Narogath where
  runMessage msg (Narogath attrs@EnemyAttrs {..}) = case msg of
    EnemySpawnEngagedWithPrey eid | eid == enemyId -> do
      playerCount <- unPlayerCount <$> getCount ()
      Narogath
        <$> runMessage msg (attrs & healthL %~ fmap (+ (3 * playerCount)))
    _ -> Narogath <$> runMessage msg attrs
