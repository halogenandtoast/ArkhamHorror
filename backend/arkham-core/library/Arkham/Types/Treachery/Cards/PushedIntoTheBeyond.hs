module Arkham.Types.Treachery.Cards.PushedIntoTheBeyond
  ( PushedIntoTheBeyond(..)
  , pushedIntoTheBeyond
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

newtype PushedIntoTheBeyond = PushedIntoTheBeyond TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pushedIntoTheBeyond :: TreacheryId -> a -> PushedIntoTheBeyond
pushedIntoTheBeyond uuid _ = PushedIntoTheBeyond $ baseAttrs uuid "02100"

instance HasModifiersFor env PushedIntoTheBeyond where
  getModifiersFor = noModifiersFor

instance HasActions env PushedIntoTheBeyond where
  getActions i window (PushedIntoTheBeyond attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env PushedIntoTheBeyond where
  runMessage msg t@(PushedIntoTheBeyond attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      storyAssets <- map unStoryAssetId <$> getSetList iid
      validAssets <- filter (`notElem` storyAssets) <$> getSetList iid
      targets <- traverse (traverseToSnd getId) validAssets
      t <$ unshiftMessage
        (chooseOne
          iid
          [ TargetLabel
              (AssetTarget aid)
              [ ShuffleIntoDeck iid (AssetTarget aid)
              , CreateEffect
                (CardCode "02100")
                (Just (EffectCardCode cardCode))
                (toSource attrs)
                (InvestigatorTarget iid)
              , Discard (toTarget attrs)
              ]
          | (aid, cardCode) <- targets
          ]
        )
    _ -> PushedIntoTheBeyond <$> runMessage msg attrs
