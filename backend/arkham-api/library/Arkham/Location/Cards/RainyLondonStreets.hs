module Arkham.Location.Cards.RainyLondonStreets (rainyLondonStreets) where

import Arkham.Ability
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (resignAction)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype RainyLondonStreets = RainyLondonStreets LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rainyLondonStreets :: LocationCard RainyLondonStreets
rainyLondonStreets = location RainyLondonStreets Cards.rainyLondonStreets 1 (PerPlayer 2)

instance HasAbilities RainyLondonStreets where
  getAbilities (RainyLondonStreets a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ DiscoveringLastClue #after Anyone (be a)
      , withI18nTooltip "rainyLondonStreets.resign" $ resignAction a
      ]

instance HasModifiersFor RainyLondonStreets where
  getModifiersFor (RainyLondonStreets attrs) = do
    n <- getCurrentActStep
    modifySelf attrs [ShroudModifier n]

instance RunMessage RainyLondonStreets where
  runMessage msg l@(RainyLondonStreets attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      n <- getPlayerCount
      placeClues (attrs.ability 1) (toTarget attrs) n
      pure l
    _ -> RainyLondonStreets <$> liftRunMessage msg attrs
