module Arkham.Treachery.Cards.SordidAndSilent (SordidAndSilent (..), sordidAndSilent) where

import Arkham.Ability
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SordidAndSilent = SordidAndSilent TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sordidAndSilent :: TreacheryCard SordidAndSilent
sordidAndSilent = treachery SordidAndSilent Cards.sordidAndSilent

instance HasAbilities SordidAndSilent where
  getAbilities (SordidAndSilent x) =
    [ mkAbility x 1 $ forced $ RoundEnds #when
    , mkAbility x 2 $ forced $ AgendaAdvances #when AnyAgenda
    ]

instance RunMessage SordidAndSilent where
  runMessage msg t@(SordidAndSilent attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ attachTreachery attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      for_ attrs.attached \case
        LocationTarget lid -> selectEach (investigatorAt lid) \iid -> assignHorror iid (attrs.ability 1) 1
        _ -> pure ()
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> SordidAndSilent <$> liftRunMessage msg attrs
