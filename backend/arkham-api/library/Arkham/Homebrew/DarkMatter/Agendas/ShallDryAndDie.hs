module Arkham.Homebrew.DarkMatter.Agendas.ShallDryAndDie (shallDryAndDie) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Matcher
import Arkham.Resolution

newtype ShallDryAndDie = ShallDryAndDie AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shallDryAndDie :: AgendaCard ShallDryAndDie
shallDryAndDie = agenda (2, A) ShallDryAndDie Cards.shallDryAndDie (Static 19)

{- | "Surface of Fragment gains: '[action]: Resign.'" Modeled as an ability the
agenda grants to anyone standing on that location, which is where the granted
ability would be usable anyway.
-}
instance HasAbilities ShallDryAndDie where
  getAbilities (ShallDryAndDie a) =
    [ restricted a 1 (OnLocation $ locationIs Locations.surfaceOfFragment)
        $ ActionAbility #resign Nothing (ActionCost 1)
    ]

instance RunMessage ShallDryAndDie where
  runMessage msg a@(ShallDryAndDie attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Resign iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      push $ ScenarioResolution NoResolution
      pure a
    _ -> ShallDryAndDie <$> liftRunMessage msg attrs
