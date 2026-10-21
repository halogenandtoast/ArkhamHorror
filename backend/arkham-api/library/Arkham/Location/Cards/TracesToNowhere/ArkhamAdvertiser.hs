module Arkham.Location.Cards.TracesToNowhere.ArkhamAdvertiser (arkhamAdvertiser) where

import Arkham.Ability
import Arkham.Helpers.Story
import Arkham.Location.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Story.CardDefs.TracesToNowhere qualified as Stories

newtype ArkhamAdvertiser = ArkhamAdvertiser LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamAdvertiser :: LocationCard ArkhamAdvertiser
arkhamAdvertiser = location ArkhamAdvertiser Cards.arkhamAdvertiser 4 (PerPlayer 2)

instance HasAbilities ArkhamAdvertiser where
  getAbilities (ArkhamAdvertiser a) =
    extendRevealed
      a
      [ groupLimit PerGame
          $ restricted a 1 Here
          $ actionAbilityWithCost (DirectHorrorCost (toSource a) You 1)
      , restricted a 2 Here actionAbility
      ]

instance RunMessage ArkhamAdvertiser where
  runMessage msg l@(ArkhamAdvertiser attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 2
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      drawEncounterCardAndThen iid (attrs.ability 2) do
        readStory iid attrs Stories.yesterdaysNews
      pure l
    _ -> ArkhamAdvertiser <$> liftRunMessage msg attrs
