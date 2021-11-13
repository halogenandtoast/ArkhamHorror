module Arkham.Types.Treachery.Cards.OozeAndFilth
  ( oozeAndFilth
  , OozeAndFilth(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype OozeAndFilth = OozeAndFilth TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oozeAndFilth :: TreacheryCard OozeAndFilth
oozeAndFilth = treachery OozeAndFilth Cards.oozeAndFilth

instance HasModifiersFor env OozeAndFilth where
  getModifiersFor _ (LocationTarget _) (OozeAndFilth a) =
    pure $ toModifiers a [ShroudModifier 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities OozeAndFilth where
  getAbilities (OozeAndFilth a) =
    [mkAbility a 1 $ ForcedAbility $ RoundEnds Timing.When]

instance TreacheryRunner env => RunMessage env OozeAndFilth where
  runMessage msg t@(OozeAndFilth attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      targetAgendas <- map AgendaTarget . setToList <$> getSet ()
      push $ chooseOrRunOne
        iid
        [ AttachTreachery (toId attrs) target | target <- targetAgendas ]
      pure t
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> OozeAndFilth <$> runMessage msg attrs
