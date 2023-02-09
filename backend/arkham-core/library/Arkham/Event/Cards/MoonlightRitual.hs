module Arkham.Event.Cards.MoonlightRitual
  ( moonlightRitual
  , MoonlightRitual(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Types ( Field (..) )
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigator.Types ( Field (..) )
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
      assets <-
        selectWithField AssetDoom $ AssetControlledBy You <> AssetWithAnyDoom
      investigatorDoomCount <- field InvestigatorDoom iid
      pushAll
        [ chooseOne iid
        $ [ targetLabel
              iid
              [RemoveDoom (InvestigatorTarget iid) investigatorDoomCount]
          | investigatorDoomCount > 0
          ]
        <> [ targetLabel aid [RemoveDoom (AssetTarget aid) assetDoomCount]
           | (aid, assetDoomCount) <- assets
           ]
        , discard attrs
        ]
      pure e
    _ -> MoonlightRitual <$> runMessage msg attrs
