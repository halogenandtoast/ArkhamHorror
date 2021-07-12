module Arkham.Types.Event.Cards.MoonlightRitual
  ( moonlightRitual
  , MoonlightRitual(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target

newtype MoonlightRitual = MoonlightRitual EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlightRitual :: EventCard MoonlightRitual
moonlightRitual = event MoonlightRitual Cards.moonlightRitual

instance HasActions env MoonlightRitual where
  getActions iid window (MoonlightRitual attrs) = getActions iid window attrs

instance HasModifiersFor env MoonlightRitual

instance
  ( HasCount DoomCount env AssetId
  , HasCount DoomCount env InvestigatorId
  , HasSet AssetId env InvestigatorId
  )
  => RunMessage env MoonlightRitual where
  runMessage msg e@(MoonlightRitual attrs) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == toId attrs -> do
      -- we assume that the only cards that are relevant here are assets and investigators
      assetIds <- getSetList @AssetId iid
      investigatorDoomCount <- unDoomCount <$> getCount iid
      assetsWithDoomCount <-
        filter ((> 0) . snd)
          <$> traverse (traverseToSnd (fmap unDoomCount . getCount)) assetIds
      e <$ pushAll
        [ chooseOne
          iid
          ([ RemoveDoom (InvestigatorTarget iid) investigatorDoomCount
           | investigatorDoomCount > 0
           ]
          <> [ RemoveDoom (AssetTarget aid) assetDoomCount
             | (aid, assetDoomCount) <- assetsWithDoomCount
             ]
          )
        , Discard (toTarget attrs)
        ]
    _ -> MoonlightRitual <$> runMessage msg attrs
