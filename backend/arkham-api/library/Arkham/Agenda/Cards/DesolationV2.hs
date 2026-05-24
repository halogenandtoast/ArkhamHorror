module Arkham.Agenda.Cards.DesolationV2 (desolationV2) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.ChaosToken
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFaces)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Trait (Trait (Insect))

newtype DesolationV2 = DesolationV2 AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desolationV2 :: AgendaCard DesolationV2
desolationV2 = agenda (2, A) DesolationV2 Cards.desolationV2 (Static 7)

instance HasModifiersFor DesolationV2 where
  getModifiersFor (DesolationV2 a) = do
    modifySelect a (EnemyWithTrait Insect) [RemoveKeyword Keyword.Aloof, LosePatrol, AddKeyword Hunter]

instance HasAbilities DesolationV2 where
  getAbilities (DesolationV2 a) =
    [ restricted
        a
        1
        ( exists (SetAsideCardMatch $ cardIs Enemies.broodQueenDyingMother)
            <> exists (VictoryDisplayCardMatch $ basic $ #enemy <> CardWithTrait Insect)
        )
        $ forced
        $ PlacedDoomCounter #after AnySource (targetIs a)
    ]

instance RunMessage DesolationV2 where
  runMessage msg a@(DesolationV2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- selectCount $ VictoryDisplayCardMatch $ basic $ #enemy <> CardWithTrait Insect
      when (n > 0) $ requestChaosTokens iid (attrs.ability 1) n
      pure a
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) _ tokens -> do
      faces <- getModifiedChaosTokenFaces tokens
      when (any isSymbolChaosToken faces) do
        lead <- getLead
        codex lead attrs Sigma
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferPhysicalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> DesolationV2 <$> liftRunMessage msg attrs
