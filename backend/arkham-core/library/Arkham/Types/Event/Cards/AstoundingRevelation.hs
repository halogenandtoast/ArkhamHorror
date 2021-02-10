module Arkham.Types.Event.Cards.AstoundingRevelation
  ( astoundingRevelation
  , AstoundingRevelation(..)
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


import Arkham.Types.Asset.Uses (UseType(..))
import Arkham.Types.Event.Attrs
import Arkham.Types.Trait

newtype AstoundingRevelation = AstoundingRevelation EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

astoundingRevelation :: InvestigatorId -> EventId -> AstoundingRevelation
astoundingRevelation iid uuid =
  AstoundingRevelation $ baseAttrs iid uuid "06023"

ability :: InvestigatorId -> EventAttrs -> Ability
ability iid a = base
  { abilityLimit = PlayerLimit (PerSearch $ Just Research) 1
  }
 where
  base = mkAbility
    (toSource a)
    1
    (ReactionAbility (DiscardCost (SearchedCardTarget iid $ getCardId a)))

instance HasActions env AstoundingRevelation where
  getActions iid (WhenAmongSearchedCards You) (AstoundingRevelation attrs) =
    pure [ActivateCardAbilityAction iid (ability iid attrs)]
  getActions iid window (AstoundingRevelation attrs) =
    getActions iid window attrs

instance HasModifiersFor env AstoundingRevelation where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasSet AssetId env (InvestigatorId, UseType)) => RunMessage env AstoundingRevelation where
  runMessage msg e@(AstoundingRevelation attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      secretAssetIds <- getSetList (iid, Secret)
      e <$ unshiftMessage
        (chooseOne
          iid
          (TakeResources iid 2 False
          : [ AddUses (AssetTarget aid) Secret 1 | aid <- secretAssetIds ]
          )
        )
    _ -> AstoundingRevelation <$> runMessage msg attrs
