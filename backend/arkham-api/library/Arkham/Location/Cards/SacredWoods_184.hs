module Arkham.Location.Cards.SacredWoods_184 (sacredWoods_184) where

import Arkham.Ability
import Arkham.Discover
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype SacredWoods_184 = SacredWoods_184 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sacredWoods_184 :: LocationCard SacredWoods_184
sacredWoods_184 = symbolLabel $ location SacredWoods_184 Cards.sacredWoods_184 4 (PerPlayer 1)

instance HasAbilities SacredWoods_184 where
  getAbilities (SacredWoods_184 a) =
    extendRevealed
      a
      [ groupLimit PerWindow
          $ restricted a 1 (exists $ investigatorAt a.id)
          $ forced
          $ PutLocationIntoPlay #after Anyone (be a)
      , restricted
          a
          2
          (Here <> youExist DeckIsEmpty <> CluesOnThis (atLeast 1) <> CanDiscoverCluesAt (be a))
          actionAbility
      ]

instance RunMessage SacredWoods_184 where
  runMessage msg l@(SacredWoods_184 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (investigatorAt attrs) \iid -> discardTopOfDeck iid (attrs.ability 1) 10
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      n <- field LocationClues attrs.id
      discoverAt NotInvestigate iid (attrs.ability 1) attrs n
      pure l
    _ -> SacredWoods_184 <$> liftRunMessage msg attrs
