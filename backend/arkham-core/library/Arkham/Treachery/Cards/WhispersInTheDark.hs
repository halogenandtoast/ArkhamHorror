module Arkham.Treachery.Cards.WhispersInTheDark (
  whispersInTheDark,
  WhispersInTheDark (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Source
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WhispersInTheDark = WhispersInTheDark TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersInTheDark :: TreacheryCard WhispersInTheDark
whispersInTheDark = treachery WhispersInTheDark Cards.whispersInTheDark

instance HasAbilities WhispersInTheDark where
  getAbilities (WhispersInTheDark a) =
    [ haunted "Take 1 horror" (proxied Anywhere a) 1
    , mkAbility a 2 $ ForcedAbility $ RoundEnds Timing.When
    ]

instance RunMessage WhispersInTheDark where
  runMessage msg t@(WhispersInTheDark attrs) = case msg of
    Revelation _iid source | isSource attrs source -> do
      agendaId <- selectJust AnyAgenda
      push $ AttachTreachery (toId attrs) (AgendaTarget agendaId)
      pure t
    UseCardAbility iid (isProxySource attrs -> True) 1 _ _ -> do
      push $ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
      pure t
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ toDiscard (toAbilitySource attrs 2) (toTarget attrs)
      pure t
    _ -> WhispersInTheDark <$> runMessage msg attrs
