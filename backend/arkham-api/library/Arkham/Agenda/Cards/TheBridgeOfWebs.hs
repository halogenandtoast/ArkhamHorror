module Arkham.Agenda.Cards.TheBridgeOfWebs (
  TheBridgeOfWebs (..),
  theBridgeOfWebs,
) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Game.Helpers (perPlayer)
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype TheBridgeOfWebs = TheBridgeOfWebs AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBridgeOfWebs :: AgendaCard TheBridgeOfWebs
theBridgeOfWebs =
  agendaWith (1, A) TheBridgeOfWebs Cards.theBridgeOfWebs (Static 7)
    $ removeDoomMatchersL
    %~ (\m -> m {removeDoomLocations = Nowhere})

instance HasModifiersFor TheBridgeOfWebs where
  getModifiersFor target (TheBridgeOfWebs attrs) | attrs `is` target = do
    n <- perPlayer 2
    pure $ toModifiers attrs [DoomThresholdModifier n]
  getModifiersFor _ _ = pure []

instance RunMessage TheBridgeOfWebs where
  runMessage msg a@(TheBridgeOfWebs attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      advanceAgendaDeck attrs
      pure a
    _ -> TheBridgeOfWebs <$> liftRunMessage msg attrs
