module Arkham.Location.Cards.VaultDoor (vaultDoor) where

import Arkham.Ability
import Arkham.Helpers.Log (remembered)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Helpers.Query (getJustLocationByName)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Move
import Arkham.ScenarioLogKey
import Arkham.Scenarios.FortuneAndFolly.Helpers

newtype VaultDoor = VaultDoor LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vaultDoor :: LocationCard VaultDoor
vaultDoor = symbolLabel $ location VaultDoor Cards.vaultDoor 3 (Static 0)

instance HasModifiersFor VaultDoor where
  getModifiersFor (VaultDoor a) = do
    n <-
      countM
        remembered
        [ WonACultistMedallion
        , ObservedTheStaff
        , FoundAbarransSigil
        , ObtainedASchematic
        , ImpersonatedAGuard
        , StayedOutOfSight
        , DeliveredADecoyPackage
        , IsamaraMesmerizedTheGuardsWithHerSong
        ]
    modifySelfWhen a (n < 4) [Blocked]

instance HasAbilities VaultDoor where
  getAbilities (VaultDoor a) =
    extendRevealed1 a $ restricted a 1 Here $ ActionAbility [#move] Nothing (ActionCost 1)

instance RunMessage VaultDoor where
  runMessage msg l@(VaultDoor attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      obtainedSchematic <- remembered ObtainedASchematic
      checkGameIcons attrs iid (if obtainedSchematic then CanMulligan 2 else NoMulligan) 1
      pure l
    DiscardedCards iid _ (isTarget attrs -> True) cards -> do
      suits <- cards & mapMaybeM toPlayingCard <&> map (.suit)
      case suits of
        (c : _) -> do
          loc <- getJustLocationByName case c of
            Hearts -> "Casino Lounge"
            Diamonds -> "Baccarat Table"
            _ -> "Guard Room"
          moveTo (attrs.ability 1) iid loc
        _ -> pure ()
      pure l
    _ -> VaultDoor <$> liftRunMessage msg attrs
