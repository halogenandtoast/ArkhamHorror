module Arkham.Location.Cards.IstanbulUniversity (istanbulUniversity) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (semaphore)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Story.Cards qualified as Stories

newtype IstanbulUniversity = IstanbulUniversity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

istanbulUniversity :: LocationCard IstanbulUniversity
istanbulUniversity = symbolLabel $ location IstanbulUniversity Cards.istanbulUniversity 4 (PerPlayer 1)

instance HasAbilities IstanbulUniversity where
  getAbilities (IstanbulUniversity a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage IstanbulUniversity where
  runMessage msg l@(IstanbulUniversity attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#willpower, #intellect] (Fixed 6)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      semaphore attrs do
        gameModifier attrs attrs Semaphore
        mCluesUnveiled <- selectOne $ storyIs Stories.theUnveiling
        for_ mCluesUnveiled $ moveTokensFrom (attrs.ability 1) iid #clue 1
      pure l
    _ -> IstanbulUniversity <$> liftRunMessage msg attrs
