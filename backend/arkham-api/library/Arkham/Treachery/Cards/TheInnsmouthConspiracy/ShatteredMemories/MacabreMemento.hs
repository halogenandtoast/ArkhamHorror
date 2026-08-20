module Arkham.Treachery.Cards.TheInnsmouthConspiracy.ShatteredMemories.MacabreMemento (macabreMemento, MacabreMemento (..)) where

import Arkham.Message.Lifted.SkillTest
import Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.ShatteredMemories qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (failOnReveal, revelationSkillTest)

newtype MacabreMemento = MacabreMemento {attrs :: TreacheryAttrs}
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Sourceable, Targetable)

macabreMemento :: TreacheryCard MacabreMemento
macabreMemento = treachery MacabreMemento Cards.macabreMemento

instance RunMessage MacabreMemento where
  runMessage msg this = runQueueT $ case msg of
    Revelation iid (isSource this -> True) -> do
      revelationSkillTest iid this #willpower (Fixed 3) $ failOnReveal #cultist
      pure this
    FailedThisSkillTest iid (isSource this -> True) -> do
      assignHorror iid this 2
      pure this
    _ -> MacabreMemento <$> liftRunMessage msg this.attrs
