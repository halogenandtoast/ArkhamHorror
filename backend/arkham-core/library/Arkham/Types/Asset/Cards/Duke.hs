module Arkham.Types.Asset.Cards.Duke
  ( Duke(..)
  , duke
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

newtype Duke = Duke AssetAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

duke :: AssetId -> Duke
duke uuid =
  Duke $ (baseAttrs uuid "02014") { assetHealth = Just 2, assetSanity = Just 3 }

instance HasModifiersFor env Duke where
  getModifiersFor (SkillTestSource _ _ source (Just Action.Fight)) (InvestigatorTarget iid) (Duke a)
    | ownedBy a iid && isSource a source
    = pure $ toModifiers a [BaseSkillOf SkillCombat 4, DamageDealt 1]
  getModifiersFor (SkillTestSource _ _ source (Just Action.Investigate)) (InvestigatorTarget iid) (Duke a)
    | ownedBy a iid && isSource a source
    = pure $ toModifiers a [BaseSkillOf SkillIntellect 4]
  getModifiersFor _ _ _ = pure []

fightAbility :: AssetAttrs -> Ability
fightAbility attrs = mkAbility
  (toSource attrs)
  1
  (ActionAbility
    (Just Action.Fight)
    (Costs [ActionCost 1, ExhaustCost (toTarget attrs)])
  )

investigateAbility :: AssetAttrs -> Ability
investigateAbility attrs = mkAbility
  (toSource attrs)
  2
  (ActionAbility
    (Just Action.Investigate)
    (Costs [ActionCost 1, ExhaustCost (toTarget attrs)])
  )

instance ActionRunner env => HasActions env Duke where
  getActions iid NonFast (Duke a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid NonFast
    investigateAvailable <- hasInvestigateActions iid NonFast
    pure
      $ [ ActivateCardAbilityAction iid (fightAbility a) | fightAvailable ]
      <> [ ActivateCardAbilityAction iid (investigateAbility a)
         | investigateAvailable
         ]
  getActions i window (Duke x) = getActions i window x

dukeInvestigate :: AssetAttrs -> InvestigatorId -> LocationId -> Message
dukeInvestigate attrs iid lid =
  Investigate iid lid (toSource attrs) SkillIntellect False

instance AssetRunner env => RunMessage env Duke where
  runMessage msg a@(Duke attrs@AssetAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      a <$ unshiftMessage (ChooseFightEnemy iid source SkillCombat False)
    UseCardAbility iid source _ 2 _ | isSource attrs source -> do
      lid <- getId iid
      accessibleLocationIds <- map unAccessibleLocationId <$> getSetList lid
      a <$ if null accessibleLocationIds
        then unshiftMessage $ dukeInvestigate attrs iid lid
        else unshiftMessage
          (chooseOne iid
          $ dukeInvestigate attrs iid lid
          : [ Run [MoveAction iid lid' False, dukeInvestigate attrs iid lid']
            | lid' <- accessibleLocationIds
            ]
          )
    _ -> Duke <$> runMessage msg attrs
