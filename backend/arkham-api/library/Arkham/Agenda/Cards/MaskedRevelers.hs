module Arkham.Agenda.Cards.MaskedRevelers (maskedRevelers) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Helpers.Window (assetLeavingPlay)
import Arkham.Matcher
import Arkham.Scenarios.TheMidwinterGala.Helpers (becomeSpellbound)
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Treacheries

newtype MaskedRevelers = MaskedRevelers AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maskedRevelers :: AgendaCard MaskedRevelers
maskedRevelers = agenda (1, A) MaskedRevelers Cards.maskedRevelers (Static 4)

instance HasModifiersFor MaskedRevelers where
  getModifiersFor (MaskedRevelers a) = do
    when (onSide A a) $ modifySelect a (EnemyWithTrait LanternClub) [CannotBeDamaged]
    modifySelf a [CannotRemoveDoomOnThis]

instance HasAbilities MaskedRevelers where
  getAbilities (MaskedRevelers a) =
    [ mkAbility a 1
        $ forced
        $ AssetWouldLeavePlay #when (AssetWithTrait Guest <> SingleSidedAsset)
    ]

instance RunMessage MaskedRevelers where
  runMessage msg a@(MaskedRevelers attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (assetLeavingPlay -> aid) _ -> do
      becomeSpellbound aid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleSetAsideIntoEncounterDeck $ oneOf [CardWithTrait Monster, cardIs Treacheries.viciousAmbush]
      shuffleEncounterDiscardBackIn
      advanceAgendaDeck attrs
      pure a
    _ -> MaskedRevelers <$> liftRunMessage msg attrs
