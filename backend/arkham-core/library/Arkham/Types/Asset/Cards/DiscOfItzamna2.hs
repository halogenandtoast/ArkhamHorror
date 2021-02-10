module Arkham.Types.Asset.Cards.DiscOfItzamna2 where

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
import Arkham.Types.Trait

newtype DiscOfItzamna2 = DiscOfItzamna2 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

discOfItzamna2 :: AssetId -> DiscOfItzamna2
discOfItzamna2 uuid =
  DiscOfItzamna2 $ (baseAttrs uuid "01059") { assetSlots = [AccessorySlot] }

instance HasModifiersFor env DiscOfItzamna2 where
  getModifiersFor = noModifiersFor

instance HasActions env DiscOfItzamna2 where
  getActions iid (WhenEnemySpawns YourLocation traits) (DiscOfItzamna2 a)
    | ownedBy a iid = pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource a) 1 (ReactionAbility Free))
      | Elite `notElem` traits
      ]
  getActions i window (DiscOfItzamna2 x) = getActions i window x

instance (AssetRunner env) => RunMessage env DiscOfItzamna2 where
  runMessage msg a@(DiscOfItzamna2 attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      unshiftMessage
        (CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers [toModifier attrs $ SkillModifier SkillWillpower 1])
          (toSource attrs)
          (InvestigatorTarget iid)
        )
      DiscOfItzamna2 <$> runMessage msg attrs
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      menemySpawnMessage <- fromQueue
        $ find ((== Just EnemySpawnMessage) . messageType)
      a <$ case menemySpawnMessage of
        Just (EnemySpawn _ _ eid) ->
          unshiftMessages [Discard (toTarget attrs), Discard (EnemyTarget eid)]
        _ -> pure ()
    _ -> DiscOfItzamna2 <$> runMessage msg attrs
