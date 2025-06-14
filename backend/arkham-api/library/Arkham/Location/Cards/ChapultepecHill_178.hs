module Arkham.Location.Cards.ChapultepecHill_178 (chapultepecHill_178) where

import Arkham.Ability
import Arkham.Discover
import Arkham.GameValue
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ChapultepecHill_178 = ChapultepecHill_178 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapultepecHill_178 :: LocationCard ChapultepecHill_178
chapultepecHill_178 = symbolLabel $ location ChapultepecHill_178 Cards.chapultepecHill_178 2 (PerPlayer 2)

instance HasAbilities ChapultepecHill_178 where
  getAbilities (ChapultepecHill_178 a) =
    extendRevealed
      a
      [ restricted a 1 (exists $ investigatorAt a.id) $ forced $ PutLocationIntoPlay #after Anyone (be a)
      , groupLimit PerRound
          $ restricted
            a
            2
            ( Here
                <> CluesOnThis (atLeast 1)
                <> CanDiscoverCluesAt (be a)
                <> youExist (HandWith $ LengthIs $ atLeast 3)
            )
          $ actionAbilityWithCost DiscardHandCost
      ]

instance RunMessage ChapultepecHill_178 where
  runMessage msg l@(ChapultepecHill_178 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (investigatorAt attrs.id) \iid ->
        randomDiscardN iid (toAbilitySource attrs 1) 2
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discoverAt NotInvestigate iid (attrs.ability 2) attrs 2
      pure l
    _ -> ChapultepecHill_178 <$> liftRunMessage msg attrs
