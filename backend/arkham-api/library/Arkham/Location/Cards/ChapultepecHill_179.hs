module Arkham.Location.Cards.ChapultepecHill_179 (chapultepecHill_179, ChapultepecHill_179 (..)) where

import Arkham.Ability
import Arkham.Discover
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait

newtype ChapultepecHill_179 = ChapultepecHill_179 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapultepecHill_179 :: LocationCard ChapultepecHill_179
chapultepecHill_179 = symbolLabel $ location ChapultepecHill_179 Cards.chapultepecHill_179 4 (PerPlayer 1)

instance HasModifiersFor ChapultepecHill_179 where
  getModifiersFor (ChapultepecHill_179 a) = do
    whenRevealed a $ modifySelect a (investigatorAt a) [SkillModifier #willpower (-2)]

instance HasAbilities ChapultepecHill_179 where
  getAbilities (ChapultepecHill_179 a) =
    extendRevealed1 a
      $ groupLimit PerPhase
      $ restricted a 1 (Here <> CluesOnThis (atLeast 1) <> CanDiscoverCluesAt (be a))
      $ freeReaction
      $ DrawCard #after You (basic $ CardWithTrait Hex) AnyDeck

instance RunMessage ChapultepecHill_179 where
  runMessage msg l@(ChapultepecHill_179 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAt NotInvestigate iid (attrs.ability 1) attrs 1
      pure l
    _ -> ChapultepecHill_179 <$> liftRunMessage msg attrs
