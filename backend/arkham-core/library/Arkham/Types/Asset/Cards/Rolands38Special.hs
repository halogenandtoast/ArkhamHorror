module Arkham.Types.Asset.Cards.Rolands38Special
  ( Rolands38Special(..)
  , rolands38Special
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


import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses

newtype Rolands38Special = Rolands38Special AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rolands38Special :: AssetId -> Rolands38Special
rolands38Special uuid =
  Rolands38Special $ (baseAttrs uuid "01006") { assetSlots = [HandSlot] }

instance HasModifiersFor env Rolands38Special where
  getModifiersFor = noModifiersFor

fightAbility :: AssetAttrs -> Ability
fightAbility attrs = mkAbility
  (toSource attrs)
  1
  (ActionAbility
    (Just Action.Fight)
    (Costs [ActionCost 1, UseCost (toId attrs) Ammo 1])
  )

instance ActionRunner env => HasActions env Rolands38Special where
  getActions iid window (Rolands38Special a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid window
    pure [ ActivateCardAbilityAction iid (fightAbility a) | fightAvailable ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env Rolands38Special where
  runMessage msg a@(Rolands38Special attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      Rolands38Special <$> runMessage msg (attrs & usesL .~ Uses Ammo 4)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      anyClues <- (> 0) . unClueCount <$> getCount locationId
      a <$ unshiftMessages
        [ CreateWindowModifierEffect EffectSkillTestWindow
          (EffectModifiers $ toModifiers
            attrs
            [ DamageDealt 1
            , SkillModifier SkillCombat (if anyClues then 3 else 1)
            ]
          )
          source
          (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillCombat False
        ]
    _ -> Rolands38Special <$> runMessage msg attrs
