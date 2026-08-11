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

{- | "Forced - At the start of the enemy phase, if The Boogeyman is at the same
location as an [[Avatar]] story asset: Place 1 doom on that story asset."
-}
instance HasAbilities It where
  getAbilities (It a) =
    [ restricted
        a
        1
        (exists $ AssetWithTrait Avatar <> AssetAt (LocationWithEnemy $ enemyIs Enemies.theBOOGEYMAN))
        $ forced
        $ PhaseBegins #when #enemy
    ]

instance RunMessage It where
  runMessage msg a@(It attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      avatars <-
        select
          $ AssetWithTrait Avatar
          <> AssetAt (LocationWithEnemy $ enemyIs Enemies.theBOOGEYMAN)
      for_ avatars \avatar -> placeDoom (attrs.ability 1) avatar 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      push $ ScenarioResolution NoResolution
      pure a
    _ -> It <$> liftRunMessage msg attrs
