module Arkham.Types.Asset.Cards.TheNecronomiconOlausWormiusTranslation
  ( theNecronomiconOlausWormiusTranslation
  , TheNecronomiconOlausWormiusTranslation(..)
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


import Arkham.Types.Asset.Attrs
import Arkham.Types.Game.Helpers

newtype TheNecronomiconOlausWormiusTranslation = TheNecronomiconOlausWormiusTranslation AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNecronomiconOlausWormiusTranslation
  :: AssetId -> TheNecronomiconOlausWormiusTranslation
theNecronomiconOlausWormiusTranslation uuid =
  TheNecronomiconOlausWormiusTranslation
    $ (baseAttrs uuid "02140") { assetSlots = [HandSlot] }

instance HasActions env TheNecronomiconOlausWormiusTranslation where
  getActions iid NonFast (TheNecronomiconOlausWormiusTranslation a)
    | ownedBy a iid = do
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility (toSource a) 1 (ActionAbility Nothing $ ActionCost 1))
        ]
  getActions _ _ _ = pure []

instance HasModifiersFor env TheNecronomiconOlausWormiusTranslation where
  getModifiersFor _ (InvestigatorTarget iid) (TheNecronomiconOlausWormiusTranslation a)
    = pure $ toModifiers a [ SkillModifier SkillIntellect 1 | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env TheNecronomiconOlausWormiusTranslation where
  runMessage msg a@(TheNecronomiconOlausWormiusTranslation attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage (TakeResources iid 2 False)
    _ -> TheNecronomiconOlausWormiusTranslation <$> runMessage msg attrs
