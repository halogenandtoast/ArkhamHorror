module Arkham.Types.Investigator.Cards.RexMurphy
  ( RexMurphy(..)
  , rexMurphy
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


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype RexMurphy = RexMurphy InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env RexMurphy where
  getModifiersFor source target (RexMurphy attrs) =
    getModifiersFor source target attrs

rexMurphy :: RexMurphy
rexMurphy = RexMurphy $ baseAttrs
  "02002"
  "Rex Murphy"
  Seeker
  Stats
    { health = 6
    , sanity = 9
    , willpower = 3
    , intellect = 4
    , combat = 2
    , agility = 3
    }
  [Reporter]

instance ActionRunner env => HasActions env RexMurphy where
  getActions iid (AfterPassSkillTest _ _ You n) (RexMurphy attrs@InvestigatorAttrs {..})
    | iid == investigatorId && n >= 2
    = do
      let ability = mkAbility (toSource attrs) 1 (ReactionAbility Free)
      clueCount' <- unClueCount <$> getCount investigatorLocation
      pure [ ActivateCardAbilityAction investigatorId ability | clueCount' > 0 ]
  getActions i window (RexMurphy attrs) = getActions i window attrs

instance HasTokenValue env RexMurphy where
  getTokenValue (RexMurphy attrs) iid ElderSign | iid == investigatorId attrs =
    pure $ TokenValue ElderSign (PositiveModifier 2)
  getTokenValue (RexMurphy attrs) iid token = getTokenValue attrs iid token

instance (InvestigatorRunner env) => RunMessage env RexMurphy where
  runMessage msg i@(RexMurphy attrs@InvestigatorAttrs {..}) = case msg of
    UseCardAbility _ (InvestigatorSource iid) _ 1 _ | iid == investigatorId ->
      i <$ unshiftMessage
        (DiscoverCluesAtLocation investigatorId investigatorLocation 1 Nothing)
    ResolveToken _drawnToken ElderSign iid | iid == investigatorId ->
      i <$ unshiftMessage
        (chooseOne
          iid
          [ Label
            "Automatically fail to draw 3"
            [FailSkillTest, DrawCards iid 3 False]
          , Label "Resolve normally" []
          ]
        )
    _ -> RexMurphy <$> runMessage msg attrs
