module Arkham.Location.Cards.TracesToNowhere.MeigerEstateBeyondTheBlackDoor (
  meigerEstateBeyondTheBlackDoor,
) where

import Arkham.Ability
import Arkham.Helpers.Story
import Arkham.Location.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.ScenarioLogKey
import Arkham.Story.CardDefs.TracesToNowhere qualified as Stories

newtype MeigerEstateBeyondTheBlackDoor = MeigerEstateBeyondTheBlackDoor LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

meigerEstateBeyondTheBlackDoor :: LocationCard MeigerEstateBeyondTheBlackDoor
meigerEstateBeyondTheBlackDoor =
  location MeigerEstateBeyondTheBlackDoor Cards.meigerEstateBeyondTheBlackDoor 1 (PerPlayer 1)

instance HasAbilities MeigerEstateBeyondTheBlackDoor where
  getAbilities (MeigerEstateBeyondTheBlackDoor a) =
    extendRevealed
      a
      [ restricted a 1 Here
          $ actionAbilityWithCost (OrCost [ClueCost (Static 2), DirectHorrorCost (toSource a) You 1])
      , restricted a 2 (Here <> Remembered TheInvestigatorsGlimpsedAnotherWorld) actionAbility
      ]

instance RunMessage MeigerEstateBeyondTheBlackDoor where
  runMessage msg l@(MeigerEstateBeyondTheBlackDoor attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readStory iid attrs Stories.acrossTheThreshold
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      selectForMaybeM (locationIs Cards.etherealPerch) $ moveTo (attrs.ability 2) iid
      pure l
    _ -> MeigerEstateBeyondTheBlackDoor <$> liftRunMessage msg attrs
