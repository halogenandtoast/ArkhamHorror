module Arkham.Homebrew.DarkMatter.Agendas.ShallDryAndDie (shallDryAndDie) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Matcher

newtype ShallDryAndDie = ShallDryAndDie AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shallDryAndDie :: AgendaCard ShallDryAndDie
shallDryAndDie = agenda (2, A) ShallDryAndDie Cards.shallDryAndDie (Static 19)

{- | "Surface of Fragment gains: '[action]: Resign.'" Proxied onto the location
so the action is anchored to (and rendered on) Surface of Fragment rather than
the agenda, while the agenda stays its owner. Both ability sweeps resolve the
'LocationMatcherSource' to the location actually in play, so the ability simply
does not exist while Surface of Fragment is not.
-}
instance HasAbilities ShallDryAndDie where
  getAbilities (ShallDryAndDie a) =
    [ restricted
        (proxied (locationIs Locations.surfaceOfFragment) a)
        1
        (OnLocation $ locationIs Locations.surfaceOfFragment)
        $ ActionAbility #resign Nothing (ActionCost 1)
    ]

instance RunMessage ShallDryAndDie where
  runMessage msg a@(ShallDryAndDie attrs) = runQueueT $ case msg of
    UseThisAbility iid (isProxySource attrs -> True) 1 -> do
      push $ Resign iid
      pure a
    -- Agenda 2b is "-> Resolution 2", regardless of the current act.
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R2
      pure a
    _ -> ShallDryAndDie <$> liftRunMessage msg attrs
