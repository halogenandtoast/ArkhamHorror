module Arkham.Types.Asset.Cards.BearTrap
  ( BearTrap(..)
  , bearTrap
  ) where

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


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype BearTrap = BearTrap AssetAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

bearTrap :: AssetId -> BearTrap
bearTrap uuid = BearTrap $ (baseAttrs uuid "81020") { assetIsStory = True }

instance HasModifiersFor env BearTrap where
  getModifiersFor _ (EnemyTarget eid) (BearTrap attrs@AssetAttrs {..})
    | Just eid == assetEnemy = pure
    $ toModifiers attrs [EnemyFight (-1), EnemyEvade (-1)]
  getModifiersFor _ _ _ = pure []

ability :: AssetAttrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (FastAbility Free)

instance HasActions env BearTrap where
  getActions iid FastPlayerWindow (BearTrap attrs) | ownedBy attrs iid = pure
    [ ActivateCardAbilityAction iid (ability attrs)
    | isNothing (assetEnemy attrs)
    ]
  getActions iid window (BearTrap x) = getActions iid window x

instance AssetRunner env => RunMessage env BearTrap where
  runMessage msg a@(BearTrap attrs@AssetAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      a <$ unshiftMessage (AttachAsset assetId (LocationTarget locationId))
    EnemyMove eid _ lid | Just lid == assetLocation -> do
      isRougarou <- (== CardCode "81028") <$> getId eid
      a <$ when
        isRougarou
        (unshiftMessage (AttachAsset assetId (EnemyTarget eid)))
    _ -> BearTrap <$> runMessage msg attrs
