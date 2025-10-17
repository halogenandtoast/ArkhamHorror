module Arkham.Treachery.Cards.CatAndMouse (catAndMouse) where

import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Helpers.Location (withLocationOf)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CatAndMouse = CatAndMouse TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catAndMouse :: TreacheryCard CatAndMouse
catAndMouse = treachery CatAndMouse Cards.catAndMouse

instance RunMessage CatAndMouse where
  runMessage msg t@(CatAndMouse attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \lid -> do
        concealed <- getConcealedAtAll (ForExpose $ toSource attrs) lid
        unless (null concealed) do
          chooseOneM iid do
            withI18n $ labeled' "skip" nothing
            for_ concealed \c -> do
              targeting c.id do
                if c.isDecoy
                  then exposeConcealed iid attrs c.id
                  else do
                    turnOverConcealed iid attrs c.id
                    chooseOneM iid $ targeting c.id $ shuffleConcealedAt lid
      pure t
    _ -> CatAndMouse <$> liftRunMessage msg attrs
