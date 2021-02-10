module Arkham.Types.Investigator.Cards.AshcanPete
  ( AshcanPete(..)
  , ashcanPete
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


import Arkham.Types.Game.Helpers
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype AshcanPete = AshcanPete InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env AshcanPete where
  getModifiersFor source target (AshcanPete attrs) =
    getModifiersFor source target attrs

ashcanPete :: AshcanPete
ashcanPete = AshcanPete $ baseAttrs
  "02005"
  "\"Ashcan\" Pete"
  Survivor
  Stats
    { health = 6
    , sanity = 5
    , willpower = 4
    , intellect = 2
    , combat = 2
    , agility = 3
    }
  [Drifter]

ability :: InvestigatorAttrs -> Ability
ability attrs = base { abilityLimit = PlayerLimit PerRound 1 }
 where
  base = mkAbility
    (toSource attrs)
    1
    (FastAbility $ HandDiscardCost 1 Nothing mempty mempty)

instance ActionRunner env => HasActions env AshcanPete where
  getActions iid FastPlayerWindow (AshcanPete attrs@InvestigatorAttrs {..})
    | iid == investigatorId = do
      exhaustedAssetIds <- map unExhaustedAssetId <$> getSetList investigatorId
      pure
        [ ActivateCardAbilityAction investigatorId (ability attrs)
        | not (null exhaustedAssetIds)
        ]
  getActions i window (AshcanPete attrs) = getActions i window attrs

instance HasTokenValue env AshcanPete where
  getTokenValue (AshcanPete attrs) iid ElderSign | iid == investigatorId attrs =
    pure $ TokenValue ElderSign (PositiveModifier 2)
  getTokenValue (AshcanPete attrs) iid token = getTokenValue attrs iid token

instance (InvestigatorRunner env) => RunMessage env AshcanPete where
  runMessage msg i@(AshcanPete attrs@InvestigatorAttrs {..}) = case msg of
    ResolveToken _drawnToken ElderSign iid | iid == investigatorId ->
      i <$ unshiftMessage (Ready $ CardCodeTarget "02014")
    UseCardAbility _ (InvestigatorSource iid) _ 1 _ | iid == investigatorId ->
      do
        exhaustedAssetIds <- map unExhaustedAssetId
          <$> getSetList investigatorId
        i <$ unshiftMessage
          (chooseOne
            investigatorId
            [ Ready (AssetTarget aid) | aid <- exhaustedAssetIds ]
          )
    SetupInvestigators -> do
      let
        (before, after) =
          break ((== "02014") . pcCardCode) (unDeck investigatorDeck)
      case after of
        (card : rest) -> do
          unshiftMessage
            (PutCardIntoPlay investigatorId (PlayerCard card) Nothing)
          AshcanPete <$> runMessage msg (attrs & deckL .~ Deck (before <> rest))
        _ -> error "Duke must be in deck"
    _ -> AshcanPete <$> runMessage msg attrs
