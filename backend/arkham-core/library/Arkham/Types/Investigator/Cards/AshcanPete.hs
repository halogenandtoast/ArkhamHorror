{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.AshcanPete where

import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
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
import ClassyPrelude
import Data.Aeson
import Lens.Micro

newtype AshcanPete = AshcanPete Attrs
  deriving newtype (Show, ToJSON, FromJSON)

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

instance ActionRunner env investigator => HasActions env investigator AshcanPete where
  getActions i FastPlayerWindow (AshcanPete Attrs {..})
    | getId () i == investigatorId = do
      let
        ability =
          (mkAbility
              (InvestigatorSource investigatorId)
              1
              (FastAbility FastPlayerWindow)
            )
            { abilityLimit = PerRound
            }
      exhaustedAssetIds <- map unExhaustedAssetId . setToList <$> asks
        (getSet investigatorId)
      usedAbilities <- map unUsedAbility <$> asks (getList ())
      pure
        [ ActivateCardAbilityAction investigatorId ability
        | (investigatorId, ability)
          `notElem` usedAbilities
          && not (null exhaustedAssetIds)
          && cardCount i
          > 0
        ]
  getActions i window (AshcanPete attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env AshcanPete where
  runMessage msg i@(AshcanPete attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid | iid == investigatorId -> do
      unshiftMessage (Ready (CardCodeTarget "02014"))
      i <$ runTest investigatorId (TokenValue ElderSign 2)
    UseCardAbility _ _ (InvestigatorSource iid) _ 1 | iid == investigatorId ->
      do
        exhaustedAssetIds <- map unExhaustedAssetId . setToList <$> asks
          (getSet investigatorId)
        i <$ unshiftMessages
          [ ChooseAndDiscardCard investigatorId
          , Ask
            investigatorId
            (ChooseOne [ Ready (AssetTarget aid) | aid <- exhaustedAssetIds ])
          ]
    SetupInvestigators -> do
      let
        (before, after) =
          break ((== "02014") . getCardCode) (unDeck investigatorDeck)
      case after of
        (card : rest) -> do
          unshiftMessage (PutCardIntoPlay investigatorId (PlayerCard card))
          AshcanPete <$> runMessage msg (attrs & deck .~ Deck (before <> rest))
        _ -> error "Duke must be in deck"
    _ -> AshcanPete <$> runMessage msg attrs
