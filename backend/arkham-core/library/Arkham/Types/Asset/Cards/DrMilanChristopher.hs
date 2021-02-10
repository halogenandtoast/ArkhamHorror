module Arkham.Types.Asset.Cards.DrMilanChristopher where

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

newtype DrMilanChristopher = DrMilanChristopher AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drMilanChristopher :: AssetId -> DrMilanChristopher
drMilanChristopher uuid =
  DrMilanChristopher
    $ baseAttrs uuid "01033"
    & (healthL ?~ 1)
    & (sanityL ?~ 2)
    & (slotsL .~ [AllySlot])

instance HasModifiersFor env DrMilanChristopher where
  getModifiersFor _ (InvestigatorTarget iid) (DrMilanChristopher a) =
    pure [ toModifier a (SkillModifier SkillIntellect 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions env DrMilanChristopher where
  getActions i (AfterSuccessfulInvestigation You _) (DrMilanChristopher x)
    | ownedBy x i = pure
      [ ActivateCardAbilityAction
          i
          (mkAbility (toSource x) 1 (ReactionAbility Free))
      ]
  getActions i window (DrMilanChristopher x) = getActions i window x

instance AssetRunner env => RunMessage env DrMilanChristopher where
  runMessage msg a@(DrMilanChristopher attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage (TakeResources iid 1 False)
    _ -> DrMilanChristopher <$> runMessage msg attrs
