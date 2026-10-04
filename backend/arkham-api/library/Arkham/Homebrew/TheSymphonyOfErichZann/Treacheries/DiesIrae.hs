module Arkham.Homebrew.TheSymphonyOfErichZann.Treacheries.DiesIrae (diesIrae) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (placeMusicTreachery, scenarioI18n)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement (Placement (NextToAgenda))
import Arkham.Treachery.Import.Lifted

newtype DiesIrae = DiesIrae TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

diesIrae :: TreacheryCard DiesIrae
diesIrae = treachery DiesIrae Cards.diesIrae

instance HasModifiersFor DiesIrae where
  -- "You cannot resign." -- only once it is actually in play next to the agenda.
  getModifiersFor (DiesIrae a) =
    modifySelect a Anyone [CannotTakeAction (IsAction #resign) | a.placement == NextToAgenda]

instance RunMessage DiesIrae where
  runMessage msg t@(DiesIrae attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 5)
      pure t
    {- "If you fail, you must either take 1 horror for each point you fail by, or
    put Dies Irae into play next to the agenda deck." -}
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      chooseOneM iid $ scenarioI18n $ scope "diesIrae" do
        countVar n $ labeled "takeHorror" $ assignHorror iid attrs n
        labeled "putIntoPlay" $ placeMusicTreachery attrs
      pure t
    _ -> DiesIrae <$> liftRunMessage msg attrs
