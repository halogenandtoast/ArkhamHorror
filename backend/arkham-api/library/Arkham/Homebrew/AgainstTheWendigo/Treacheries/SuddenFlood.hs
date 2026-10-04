module Arkham.Homebrew.AgainstTheWendigo.Treacheries.SuddenFlood (suddenFlood) where

import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.AgainstTheWendigo.Actions (pattern Navigate)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (River))
import Arkham.Treachery.Import.Lifted

newtype SuddenFlood = SuddenFlood TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

suddenFlood :: TreacheryCard SuddenFlood
suddenFlood = treachery SuddenFlood Cards.suddenFlood

instance RunMessage SuddenFlood where
  runMessage msg t@(SuddenFlood attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      selectEach (InvestigatorAt $ LocationWithTrait River) \iid -> do
        sid <- getRandom
        chooseSkillM iid [#combat, #agility] \sType ->
          revelationSkillTest sid iid attrs sType (Fixed 4)
        -- "Until the end of the round, investigators on River locations cannot
        -- perform Navigate actions."
        roundModifier attrs iid (CannotTakeAction $ IsAction Navigate)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 2
      pure t
    _ -> SuddenFlood <$> liftRunMessage msg attrs
