module Arkham.Treachery.Cards.HuntedByByakhee (huntedByByakhee) where

import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Message.Lifted.Choose
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HuntedByByakhee = HuntedByByakhee TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntedByByakhee :: TreacheryCard HuntedByByakhee
huntedByByakhee = treachery HuntedByByakhee Cards.huntedByByakhee

instance RunMessage HuntedByByakhee where
  runMessage msg t@(HuntedByByakhee attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 6)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      discardTopOfEncounterDeckAndHandle iid attrs n attrs
      shuffleDeck Deck.EncounterDeck
      pure t
    DiscardedTopOfEncounterDeck iid cards _ (isTarget attrs -> True) -> do
      let
        isByakhee = member Byakhee . cdCardTraits . toCardDef
        isOmen = member Omen . cdCardTraits . toCardDef
        byakhee = filter isByakhee cards
        omens = filter isOmen cards
      focusCards cards do
        if null byakhee
          then continue_ iid
          else chooseTargetM iid byakhee (drawCard iid)

      unless (null omens) $ assignHorror iid attrs 1
      pure t
    _ -> HuntedByByakhee <$> liftRunMessage msg attrs
