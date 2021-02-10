module Arkham.Types.Treachery.Cards.TerrorFromBeyond
  ( TerrorFromBeyond(..)
  , terrorFromBeyond
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


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype TerrorFromBeyond = TerrorFromBeyond TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorFromBeyond :: TreacheryId -> a -> TerrorFromBeyond
terrorFromBeyond uuid _ = TerrorFromBeyond $ baseAttrs uuid "02101"

instance HasModifiersFor env TerrorFromBeyond where
  getModifiersFor = noModifiersFor

instance HasActions env TerrorFromBeyond where
  getActions i window (TerrorFromBeyond attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env TerrorFromBeyond where
  runMessage msg t@(TerrorFromBeyond attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      iids <- getSetList ()
      phaseHistory <- getPhaseHistory =<< ask
      let
        secondCopy =
          count
              (\case
                DrewTreachery _ card | getCardCode card == "02101" -> True
                _ -> False
              )
              phaseHistory
            >= 2
      iidsWithAssets <- traverse
        (traverseToSnd $ (map unHandCardId <$>) . getSetList . (, AssetType))
        iids
      iidsWithEvents <- traverse
        (traverseToSnd $ (map unHandCardId <$>) . getSetList . (, EventType))
        iids
      iidsWithSkills <- traverse
        (traverseToSnd $ (map unHandCardId <$>) . getSetList . (, SkillType))
        iids
      t <$ unshiftMessages
        [ chooseN
          iid
          (if secondCopy then 2 else 1)
          [ Label
            "Assets"
            [ Run [ DiscardCard iid' aid | aid <- assets ]
            | (iid', assets) <- iidsWithAssets
            ]
          , Label
            "Events"
            [ Run [ DiscardCard iid' eid | eid <- events ]
            | (iid', events) <- iidsWithEvents
            ]
          , Label
            "Skills"
            [ Run [ DiscardCard iid' sid | sid <- skills ]
            | (iid', skills) <- iidsWithSkills
            ]
          ]
        , Discard $ TreacheryTarget (treacheryId attrs)
        ]
    _ -> TerrorFromBeyond <$> runMessage msg attrs
