module Arkham.Event.Cards.MoonlightRitual
  ( moonlightRitual
  , MoonlightRitual(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Query
import Arkham.Target

newtype MoonlightRitual = MoonlightRitual EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlightRitual :: EventCard MoonlightRitual
moonlightRitual = event MoonlightRitual Cards.moonlightRitual

instance
  ( HasCount DoomCount env AssetId
  , HasCount DoomCount env InvestigatorId
  , Query AssetMatcher env
  )
  => RunMessage env MoonlightRitual where
  runMessage msg e@(MoonlightRitual attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      -- we assume that the only cards that are relevant here are assets and investigators
      assetIds <- selectList (AssetControlledBy You)
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
