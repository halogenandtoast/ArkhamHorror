module Arkham.Types.Asset.Cards.TheNecronomiconAdvanced
  ( TheNecronomiconAdvanced(..)
  , theNecronomiconAdvanced
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
import qualified Arkham.Types.Token as Token

newtype TheNecronomiconAdvanced = TheNecronomiconAdvanced AssetAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

theNecronomiconAdvanced :: AssetId -> TheNecronomiconAdvanced
theNecronomiconAdvanced uuid =
  TheNecronomiconAdvanced $ (baseAttrs uuid "90003")
    { assetSlots = [HandSlot]
    , assetHorror = Just 3
    , assetCanLeavePlayByNormalMeans = False
    }

instance HasModifiersFor env TheNecronomiconAdvanced where
  getModifiersFor _ (InvestigatorTarget iid) (TheNecronomiconAdvanced a) =
    pure $ toModifiers
      a
      [ ForcedTokenChange
          Token.ElderSign
          [Token.Cultist, Token.Tablet, Token.ElderThing]
      | ownedBy a iid
      ]
  getModifiersFor _ _ _ = pure []

instance HasActions env TheNecronomiconAdvanced where
  getActions iid NonFast (TheNecronomiconAdvanced a) | ownedBy a iid = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility (toSource a) 1 (ActionAbility Nothing $ ActionCost 1))
    | fromJustNote "Must be set" (assetHorror a) > 0
    ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env TheNecronomiconAdvanced where
  runMessage msg a@(TheNecronomiconAdvanced attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      a <$ unshiftMessage (PlayCard iid (getCardId attrs) Nothing False)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      unshiftMessage $ InvestigatorDamage iid source 0 1
      if fromJustNote "Must be set" (assetHorror attrs) == 1
        then a <$ unshiftMessage (Discard (toTarget attrs))
        else pure $ TheNecronomiconAdvanced
          (attrs { assetHorror = max 0 . subtract 1 <$> assetHorror attrs })
    _ -> TheNecronomiconAdvanced <$> runMessage msg attrs
