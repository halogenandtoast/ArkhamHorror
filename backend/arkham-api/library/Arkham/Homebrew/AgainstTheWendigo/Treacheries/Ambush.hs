module Arkham.Homebrew.AgainstTheWendigo.Treacheries.Ambush (ambush) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Trait (Trait (River))
import Arkham.Treachery.Import.Lifted

newtype Ambush = Ambush TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ambush :: TreacheryCard Ambush
ambush = treachery Ambush Cards.ambush

instance HasModifiersFor Ambush where
  {- "If you are on a River location, Ambush gains the River trait. (This effect
  may trigger an ability on some Guide asset.)" -- the Guides' second reaction
  answers a test a River treachery starts, which is what this is for. -}
  getModifiersFor (Ambush a) = do
    onTheRiver <- selectAny $ InvestigatorWithId a.drawnBy <> at_ (LocationWithTrait River)
    modifySelfWhen a onTheRiver [AddTrait River]

instance RunMessage Ambush where
  runMessage msg t@(Ambush attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 1
      pure t
    _ -> Ambush <$> liftRunMessage msg attrs
