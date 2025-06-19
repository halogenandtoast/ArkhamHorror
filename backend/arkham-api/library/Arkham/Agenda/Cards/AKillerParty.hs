module Arkham.Agenda.Cards.AKillerParty (aKillerParty) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Helpers.Window (assetLeavingPlay)
import Arkham.Matcher
import Arkham.Scenarios.TheMidwinterGala.Helpers (becomeSpellbound)
import Arkham.Trait

newtype AKillerParty = AKillerParty AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aKillerParty :: AgendaCard AKillerParty
aKillerParty = agenda (3, A) AKillerParty Cards.aKillerParty (Static 6)

instance HasModifiersFor AKillerParty where
  getModifiersFor (AKillerParty a) = do
    when (onSide A a)
      $ modifySelect
        a
        Anyone
        [CannotTriggerAbilityMatching $ AbilityOnEnemy (EnemyWithTrait LanternClub) <> #parley]
    modifySelf a [CannotRemoveDoomOnThis]

instance HasAbilities AKillerParty where
  getAbilities (AKillerParty a) =
    [ mkAbility a 1
        $ forced
        $ AssetWouldLeavePlay #when (AssetWithTrait Guest <> SingleSidedAsset)
    ]

instance RunMessage AKillerParty where
  runMessage msg a@(AKillerParty attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws@(assetLeavingPlay -> aid) _ -> do
      cancelWindowBatch ws
      becomeSpellbound aid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferMentalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> AKillerParty <$> liftRunMessage msg attrs
