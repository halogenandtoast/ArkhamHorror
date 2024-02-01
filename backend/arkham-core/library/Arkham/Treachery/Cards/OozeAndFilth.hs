module Arkham.Treachery.Cards.OozeAndFilth (
  oozeAndFilth,
  OozeAndFilth (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype OozeAndFilth = OozeAndFilth TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

oozeAndFilth :: TreacheryCard OozeAndFilth
oozeAndFilth = treachery OozeAndFilth Cards.oozeAndFilth

instance HasModifiersFor OozeAndFilth where
  getModifiersFor (LocationTarget _) (OozeAndFilth a) =
    pure $ toModifiers a [ShroudModifier 1]
  getModifiersFor _ _ = pure []

instance HasAbilities OozeAndFilth where
  getAbilities (OozeAndFilth a) =
    [mkAbility a 1 $ ForcedAbility $ RoundEnds #when]

instance RunMessage OozeAndFilth where
  runMessage msg t@(OozeAndFilth attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      targetAgendas <- selectListMap AgendaTarget AnyAgenda
      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
          [ TargetLabel target [attachTreachery attrs target]
          | target <- targetAgendas
          ]
      pure t
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ toDiscard (toAbilitySource attrs 1) attrs
      pure t
    _ -> OozeAndFilth <$> runMessage msg attrs
