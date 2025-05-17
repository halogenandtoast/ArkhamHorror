module Arkham.Location.Cards.Gallery (gallery) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype Gallery = Gallery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gallery :: LocationCard Gallery
gallery = location Gallery Cards.gallery 1 (Static 0)

instance HasAbilities Gallery where
  getAbilities (Gallery a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here $ forced $ TurnEnds #after You

instance RunMessage Gallery where
  runMessage msg l@(Gallery attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 2)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      clueCount <- field InvestigatorClues iid
      when (clueCount > 0) $ moveTokens (attrs.ability 1) iid attrs #clue 1
      pure l
    _ -> Gallery <$> liftRunMessage msg attrs
