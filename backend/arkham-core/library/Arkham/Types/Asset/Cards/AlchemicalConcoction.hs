module Arkham.Types.Asset.Cards.AlchemicalConcoction
  ( alchemicalConcoction
  , AlchemicalConcoction(..)
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

newtype AlchemicalConcoction = AlchemicalConcoction AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemicalConcoction :: AssetId -> AlchemicalConcoction
alchemicalConcoction uuid = AlchemicalConcoction $ baseAttrs uuid "02059"

instance ActionRunner env => HasActions env AlchemicalConcoction where
  getActions iid window (AlchemicalConcoction a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid window
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility
            (toSource a)
            1
            (ActionAbility (Just Action.Fight) $ ActionCost 1)
          )
      | fightAvailable
      ]
  getActions _ _ _ = pure []

instance (HasId CardCode env EnemyId, HasTarget ForSkillTest env) => HasModifiersFor env AlchemicalConcoction where
  getModifiersFor (SkillTestSource _ _ source (Just Action.Fight)) _ (AlchemicalConcoction a)
    | isSource a source
    = do
      skillTestTarget <-
        asks $ fromJustNote "not a skilltest" . getTarget ForSkillTest
      case skillTestTarget of
        EnemyTarget eid -> do
          cardCode <- getId eid
          pure $ toModifiers a [ DamageDealt 6 | cardCode == CardCode "02059" ]
        _ -> pure []
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env AlchemicalConcoction where
  runMessage msg a@(AlchemicalConcoction attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      a <$ unshiftMessages
        [ CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [DamageDealt 1])
          source
          (InvestigatorTarget iid)
        , CreateEffect "01060" Nothing source (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillWillpower False
        ]
    _ -> AlchemicalConcoction <$> runMessage msg attrs
