module Arkham.Homebrew.ThePhantomOfTheOpera.Treacheries.InsidiousTrick (insidiousTrick) where

import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Treacheries qualified as Cards
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype InsidiousTrick = InsidiousTrick TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

insidiousTrick :: TreacheryCard InsidiousTrick
insidiousTrick = treachery InsidiousTrick Cards.insidiousTrick

instance RunMessage InsidiousTrick where
  runMessage msg t@(InsidiousTrick attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      chooseRevelationSkillTest sid iid attrs [#willpower, #agility] (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      connected <- select $ connectedFrom (locationWithInvestigator iid)
      counts <- for connected \lid -> (lid,) <$> fieldMap LocationConcealedCards length lid
      let fewest = [lid | (lid, n) <- counts, all ((>= n) . snd) counts]
      unless (null fewest) $ chooseTargetM iid fewest $ moveTo attrs iid
      assignDamageOrHorror iid attrs 1 1
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      withLocationOf iid \lid -> do
        concealed <- getConcealedAt NotForExpose lid
        unless (null concealed) $ chooseRevealConcealedAt iid attrs (LocationWithId lid)
      pure t
    _ -> InsidiousTrick <$> liftRunMessage msg attrs
