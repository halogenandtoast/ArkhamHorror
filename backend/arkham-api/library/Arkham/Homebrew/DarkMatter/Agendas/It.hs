module Arkham.Homebrew.DarkMatter.Agendas.It (it) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Matcher
import Arkham.Resolution
import Arkham.Trait (Trait (Avatar))

newtype It = It AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

it :: AgendaCard It
it = agenda (2, A) It Cards.it (Static 10)

instance HasAbilities It where
  getAbilities (It a) =
    [ restricted
        a
        1
        (exists $ AssetWithTrait Avatar <> at_ (LocationWithEnemy $ enemyIs Enemies.theBOOGEYMAN))
        $ forced
        $ PhaseBegins #when #enemy
    ]

instance RunMessage It where
  runMessage msg a@(It attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (AssetWithTrait Avatar <> at_ (LocationWithEnemy $ enemyIs Enemies.theBOOGEYMAN))
        $ placeDoomOn (attrs.ability 1) 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      push $ ScenarioResolution NoResolution
      pure a
    _ -> It <$> liftRunMessage msg attrs
