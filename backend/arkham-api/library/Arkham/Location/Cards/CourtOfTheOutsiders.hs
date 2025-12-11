module Arkham.Location.Cards.CourtOfTheOutsiders (courtOfTheOutsiders) where

import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (resignAction)
import Arkham.Location.Import.Lifted
import Arkham.Location.Runner (pattern UseResign)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Conspirator))

newtype CourtOfTheOutsiders = CourtOfTheOutsiders LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

courtOfTheOutsiders :: LocationCard CourtOfTheOutsiders
courtOfTheOutsiders =
  symbolLabel
    $ locationWith CourtOfTheOutsiders Cards.courtOfTheOutsiders 2 (PerPlayer 2) connectsToAdjacent

instance HasAbilities CourtOfTheOutsiders where
  getAbilities (CourtOfTheOutsiders a) =
    extendRevealed1 a $ resignAction a

instance RunMessage CourtOfTheOutsiders where
  runMessage msg l@(CourtOfTheOutsiders attrs) = runQueueT $ case msg of
    UseResign iid (isSource attrs -> True) -> do
      sid <- getRandom
      mods <- getModifiers attrs
      let n = max 0 (4 + sum [x | Difficulty x <- mods])
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#willpower, #agility] (Fixed n)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      selectEach (assetControlledBy iid <> AssetWithTrait Conspirator) (addToVictory iid)
      resign iid
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      turnModifier iid attrs attrs (Difficulty (-1))
      pure l
    _ -> CourtOfTheOutsiders <$> liftRunMessage msg attrs
