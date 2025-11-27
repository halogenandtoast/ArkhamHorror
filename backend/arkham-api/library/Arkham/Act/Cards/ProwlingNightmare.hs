module Arkham.Act.Cards.ProwlingNightmare (prowlingNightmare) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Location (addDirectConnection, withLocationOf)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move

newtype ProwlingNightmare = ProwlingNightmare ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prowlingNightmare :: ActCard ProwlingNightmare
prowlingNightmare = act (3, A) ProwlingNightmare Cards.prowlingNightmare Nothing

instance HasAbilities ProwlingNightmare where
  getAbilities = actAbilities1 \a ->
    mkAbility a 1 $ Objective $ forced $ afterExposed Enemies.voidChimeraTrueForm

instance RunMessage ProwlingNightmare where
  runMessage msg a@(ProwlingNightmare attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      voidChimera <- selectJust $ enemyIs Enemies.voidChimeraTrueForm
      outsidersLair <- placeLocation Locations.outsidersLair
      withLocationOf voidChimera \loc -> do
        addDirectConnection loc outsidersLair
        enemyMoveTo attrs voidChimera outsidersLair
        selectEach (investigatorAt loc) \iid -> moveTo attrs iid outsidersLair
      advanceActDeck attrs
      pure a
    _ -> ProwlingNightmare <$> liftRunMessage msg attrs
