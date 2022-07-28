module Arkham.Event.Cards.MoonlightRitual
  ( moonlightRitual
  , MoonlightRitual(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Asset.Types ( Field(..) )
import Arkham.Investigator.Attrs ( Field(..) )
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Target

newtype MoonlightRitual = MoonlightRitual EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlightRitual :: EventCard MoonlightRitual
moonlightRitual = event MoonlightRitual Cards.moonlightRitual

instance RunMessage MoonlightRitual where
  runMessage msg e@(MoonlightRitual attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      -- we assume that the only cards that are relevant here are assets and investigators
      assetIds <- selectList (AssetControlledBy You)
      investigatorDoomCount <- field InvestigatorDoom iid
      assetsWithDoomCount <-
        filter ((> 0) . snd)
          <$> traverse (traverseToSnd (field AssetDoom)) assetIds
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
