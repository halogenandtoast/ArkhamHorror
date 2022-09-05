module Arkham.Treachery.Cards.DeepDark
  ( deepDark
  , DeepDark(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DeepDark = DeepDark TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepDark :: TreacheryCard DeepDark
deepDark = treachery DeepDark Cards.deepDark

instance HasAbilities DeepDark where
  getAbilities (DeepDark a) =
    [ limitedAbility (PerCopyLimit Cards.deepDark PerRound 1)
        $ mkAbility a 1
        $ ForcedAbility
        $ RoundEnds Timing.When
    ]

instance RunMessage DeepDark where
  runMessage msg t@(DeepDark attrs) = case msg of
    Revelation _iid source | isSource attrs source -> do
      push $ PlaceTreachery (toId attrs) TreacheryNextToAct
      pure t
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      push $ Discard (toTarget attrs)
      pure t
    _ -> DeepDark <$> runMessage msg attrs
