module Arkham.Types.Investigator.Cards.AshcanPete
  ( AshcanPete(..)
  , ashcanPete
  )
where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Helpers
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.Window

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

instance InvestigatorRunner env => HasActions env AshcanPete where
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
          break ((== "02014") . cdCardCode . pcDef) (unDeck investigatorDeck)
      case after of
        (card : rest) -> do
          unshiftMessage
            (PutCardIntoPlay investigatorId (PlayerCard card) Nothing)
          AshcanPete <$> runMessage msg (attrs & deckL .~ Deck (before <> rest))
        _ -> error "Duke must be in deck"
    _ -> AshcanPete <$> runMessage msg attrs
