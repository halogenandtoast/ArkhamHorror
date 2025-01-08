module Arkham.Location.Cards.TheSummit (theSummit) where

import Arkham.Ability
import Arkham.Constants
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (resignAction)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Expedition))

newtype TheSummit = TheSummit LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSummit :: LocationCard TheSummit
theSummit = locationWith TheSummit Cards.theSummit 3 (PerPlayer 3) (connectsToL .~ adjacentLocations)

instance HasAbilities TheSummit where
  getAbilities (TheSummit a) =
    extendRevealed
      a
      [ restricted a 1 HasRemainingFrostTokens $ forced $ RevealLocation #after Anyone (be a)
      , resignAction a
      ]

instance HasModifiersFor TheSummit where
  getModifiersFor (TheSummit l) = do
    whenUnrevealed l $ blockedWhenAny l $ leftOf l <> LocationWithAnyClues
    modifySelect l Anyone [CannotDiscoverCluesExceptAsResultOfInvestigation (be l)]

instance RunMessage TheSummit where
  runMessage msg l@(TheSummit attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      addChaosToken #frost
      pure l
    UseThisAbility iid (isSource attrs -> True) ResignAbility -> do
      selectEach (assetControlledBy iid <> withTrait Expedition) addToVictory
      discardAllClues (attrs.ability 1) iid
      TheSummit <$> liftRunMessage msg attrs
    _ -> TheSummit <$> liftRunMessage msg attrs
