module Arkham.Agenda.Cards.UnsettlingSilence (unsettlingSilence) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.ChaosToken
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFaces)
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Trait (Trait (Insect))

newtype UnsettlingSilence = UnsettlingSilence AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unsettlingSilence :: AgendaCard UnsettlingSilence
unsettlingSilence = agenda (1, A) UnsettlingSilence Cards.unsettlingSilence (Static 8)

instance HasAbilities UnsettlingSilence where
  getAbilities (UnsettlingSilence a) =
    [ restricted
        a
        1
        ( exists (SetAsideCardMatch $ cardIs Enemies.broodQueenDyingMother)
            <> exists (VictoryDisplayCardMatch $ basic $ #enemy <> CardWithTrait Insect)
        )
        $ forced
        $ PlacedDoomCounter #after AnySource (targetIs a)
    ]

instance RunMessage UnsettlingSilence where
  runMessage msg a@(UnsettlingSilence attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- selectCount $ VictoryDisplayCardMatch $ basic $ #enemy <> CardWithTrait Insect
      when (n > 0) $ requestChaosTokens iid (attrs.ability 1) n
      pure a
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) _ tokens -> do
      faces <- getModifiedChaosTokenFaces tokens
      when (any (`elem` [Skull, AutoFail]) faces) do
        lead <- getLead
        codex lead attrs Sigma
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> UnsettlingSilence <$> liftRunMessage msg attrs
