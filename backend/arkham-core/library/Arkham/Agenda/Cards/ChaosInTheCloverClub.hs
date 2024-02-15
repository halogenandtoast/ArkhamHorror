module Arkham.Agenda.Cards.ChaosInTheCloverClub (
  ChaosInTheCloverClub (..),
  chaosInTheCloverClub,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Trait

newtype ChaosInTheCloverClub = ChaosInTheCloverClub AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chaosInTheCloverClub :: AgendaCard ChaosInTheCloverClub
chaosInTheCloverClub =
  agenda (3, A) ChaosInTheCloverClub Cards.chaosInTheCloverClub (Static 7)

instance HasAbilities ChaosInTheCloverClub where
  getAbilities (ChaosInTheCloverClub x) = [mkAbility x 1 $ ForcedAbility $ PhaseBegins #when #enemy | onSide A x]

instance RunMessage ChaosInTheCloverClub where
  runMessage msg a@(ChaosInTheCloverClub attrs@AgendaAttrs {..}) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      criminals <-
        select $ EnemyWithTrait Criminal <> EnemyAt (LocationWithEnemy $ EnemyWithTrait Abomination)
      pushAll $ map (toDiscard attrs) criminals
      pure a
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      lead <- getLeadPlayer
      push $ chooseOne lead [Label "Continue" [R4]]
      pure a
    _ -> ChaosInTheCloverClub <$> runMessage msg attrs
