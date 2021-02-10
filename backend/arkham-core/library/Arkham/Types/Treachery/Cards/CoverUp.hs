module Arkham.Types.Treachery.Cards.CoverUp
  ( CoverUp(..)
  , coverUp
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


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype CoverUp = CoverUp TreacheryAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

coverUp :: TreacheryId -> Maybe InvestigatorId -> CoverUp
coverUp uuid iid =
  CoverUp $ (weaknessAttrs uuid iid "01007") { treacheryClues = Just 3 }

coverUpClues :: TreacheryAttrs -> Int
coverUpClues TreacheryAttrs { treacheryClues } =
  fromJustNote "must be set" treacheryClues

instance HasModifiersFor env CoverUp where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env CoverUp where
  getActions iid (WhenDiscoverClues You YourLocation) (CoverUp a@TreacheryAttrs {..})
    = withTreacheryInvestigator a $ \tormented -> do
      treacheryLocationId <- getId @LocationId tormented
      investigatorLocationId <- getId @LocationId iid
      cluesToDiscover <- fromQueue $ \queue -> do
        let
          mDiscoverClues = flip find queue $ \case
            DiscoverClues{} -> True
            _ -> False
        case mDiscoverClues of
          Just (DiscoverClues _ _ m _) -> m
          _ -> 0
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility (toSource a) 1 (ReactionAbility Free))
        | (treacheryLocationId == investigatorLocationId)
          && (coverUpClues a > 0)
          && (cluesToDiscover > 0)
        ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env CoverUp where
  runMessage msg t@(CoverUp attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> t <$ unshiftMessages
      [ RemoveCardFromHand iid "01007"
      , AttachTreachery treacheryId (InvestigatorTarget iid)
      ]
    InvestigatorEliminated iid | treacheryOnInvestigator iid attrs ->
      runMessage EndOfGame t >>= \case
        CoverUp attrs' -> CoverUp <$> runMessage msg attrs'
    EndOfGame | coverUpClues attrs > 0 -> withTreacheryInvestigator attrs
      $ \tormented -> t <$ unshiftMessage (SufferTrauma tormented 0 1)
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      cluesToRemove <- withQueue $ \queue -> do
        let
          (before, after) = flip break queue $ \case
            DiscoverClues{} -> True
            _ -> False
          (DiscoverClues _ _ m _, remaining) = case after of
            [] -> error "DiscoverClues has to be present"
            (x : xs) -> (x, xs)
        (before <> remaining, m)
      let remainingClues = max 0 (coverUpClues attrs - cluesToRemove)
      pure $ CoverUp (attrs { treacheryClues = Just remainingClues })
    _ -> CoverUp <$> runMessage msg attrs
