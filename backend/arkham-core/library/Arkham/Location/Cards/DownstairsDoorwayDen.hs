module Arkham.Location.Cards.DownstairsDoorwayDen (downstairsDoorwayDen, DownstairsDoorwayDen (..)) where

import Arkham.Discover
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.ScenarioLogKey (ScenarioLogKey (StudiedADesecratedPortrait))

newtype DownstairsDoorwayDen = DownstairsDoorwayDen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downstairsDoorwayDen :: LocationCard DownstairsDoorwayDen
downstairsDoorwayDen = location DownstairsDoorwayDen Cards.downstairsDoorwayDen 4 (PerPlayer 2)

instance HasAbilities DownstairsDoorwayDen where
  getAbilities (DownstairsDoorwayDen attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 1 (Here <> CluesOnThis (atLeast 1) <> CanDiscoverCluesAt (be attrs))
          $ ReactionAbility
            (SkillTestResult #after You (whileInvestigating attrs) #success)
            (HandDiscardCost 1 AnyCard)
      , restrictedAbility attrs 2 Here
          $ FastAbility
          $ GroupClueCost (PerPlayer 1) (LocationWithId $ toId attrs)
      ]

instance RunMessage DownstairsDoorwayDen where
  runMessage msg l@(DownstairsDoorwayDen attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Msg.DiscoverClues iid $ discover attrs (attrs.ability 1) 1
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ Remember StudiedADesecratedPortrait
      pure l
    _ -> DownstairsDoorwayDen <$> runMessage msg attrs
