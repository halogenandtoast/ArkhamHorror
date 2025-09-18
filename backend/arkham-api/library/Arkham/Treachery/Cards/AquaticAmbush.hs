module Arkham.Treachery.Cards.AquaticAmbush (aquaticAmbush) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTest, isFightWith)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AquaticAmbush = AquaticAmbush TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aquaticAmbush :: TreacheryCard AquaticAmbush
aquaticAmbush = treachery AquaticAmbush Cards.aquaticAmbush

instance HasModifiersFor AquaticAmbush where
  getModifiersFor (AquaticAmbush a) =
    whenJustM getSkillTest \st -> maybeModified_ a (SkillTestTarget st.id) do
      liftGuardM $ matches st.investigator (not_ $ InVehicleMatching $ assetIs Assets.fishingVessel)
      liftGuardM $ isFightWith (EnemyAt FloodedLocation)
      pure [RevealAnotherChaosToken]

instance HasAbilities AquaticAmbush where
  getAbilities (AquaticAmbush a) =
    [limited (MaxPer Cards.aquaticAmbush PerRound 1) $ mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage AquaticAmbush where
  runMessage msg t@(AquaticAmbush attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> AquaticAmbush <$> liftRunMessage msg attrs
