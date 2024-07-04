module Arkham.Treachery.Cards.WhispersInTheDark (whispersInTheDark, WhispersInTheDark (..)) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WhispersInTheDark = WhispersInTheDark TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersInTheDark :: TreacheryCard WhispersInTheDark
whispersInTheDark = treachery WhispersInTheDark Cards.whispersInTheDark

instance HasAbilities WhispersInTheDark where
  getAbilities (WhispersInTheDark a) =
    [ haunted "Take 1 horror" (proxied Anywhere a) 1
    , mkAbility a 2 $ ForcedAbility $ RoundEnds #when
    ]

instance RunMessage WhispersInTheDark where
  runMessage msg t@(WhispersInTheDark attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      selectJust AnyAgenda >>= attachTreachery attrs
      pure t
    UseThisAbility iid (isProxySource attrs -> True) 1 -> do
      assignHorror iid attrs 1
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> WhispersInTheDark <$> liftRunMessage msg attrs
