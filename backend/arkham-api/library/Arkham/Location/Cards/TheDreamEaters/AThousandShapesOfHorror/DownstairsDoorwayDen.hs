module Arkham.Location.Cards.TheDreamEaters.AThousandShapesOfHorror.DownstairsDoorwayDen (downstairsDoorwayDen, DownstairsDoorwayDen (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.CardDefs.TheDreamEaters.AThousandShapesOfHorror qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Log
import Arkham.Matcher
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
      [ restricted attrs 1 (Here <> canDiscoverCluesAt (be attrs))
          $ triggered
            (SkillTestResult #after You (whileInvestigating attrs) #success)
            (HandDiscardCost 1 #any)
      , onlyOnce
          $ restricted attrs 2 Here
          $ FastAbility
          $ GroupClueCost (PerPlayer 1) (LocationWithId $ toId attrs)
      ]

instance RunMessage DownstairsDoorwayDen where
  runMessage msg l@(DownstairsDoorwayDen attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      remember StudiedADesecratedPortrait
      pure l
    _ -> DownstairsDoorwayDen <$> liftRunMessage msg attrs
