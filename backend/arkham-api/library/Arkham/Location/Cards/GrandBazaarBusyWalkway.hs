module Arkham.Location.Cards.GrandBazaarBusyWalkway (grandBazaarBusyWalkway) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Window (getBatchId)

newtype GrandBazaarBusyWalkway = GrandBazaarBusyWalkway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandBazaarBusyWalkway :: LocationCard GrandBazaarBusyWalkway
grandBazaarBusyWalkway =
  locationWith GrandBazaarBusyWalkway Cards.grandBazaarBusyWalkway 1 (PerPlayer 1) connectsToAdjacent

instance HasAbilities GrandBazaarBusyWalkway where
  getAbilities (GrandBazaarBusyWalkway a) =
    extendRevealed1 a
      $ skillTestAbility
      $ forcedAbility a 1
      $ WouldMove #when You #any (be a) Anywhere

instance RunMessage GrandBazaarBusyWalkway where
  runMessage msg l@(GrandBazaarBusyWalkway attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getBatchId -> batchId) _ -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) (BatchTarget batchId) #agility (Fixed 3)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      actions <- field InvestigatorRemainingActions iid
      chooseOneM iid $ withI18n do
        countVar 1 $ labeledValidate' (actions > 0) "loseActions" $ loseActions iid (attrs.ability 1) 1
        labeled' "cancelMove" $ cancelMovement (attrs.ability 1) iid
      pure l
    _ -> GrandBazaarBusyWalkway <$> liftRunMessage msg attrs
