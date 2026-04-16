module Arkham.Treachery.Cards.DeepShadows (deepShadows) where

import Arkham.Asset.Types (Field (..))
import Arkham.ForMovement
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Projection
import Arkham.Scenarios.TheTwistedHollow.Helpers
import Arkham.Trait (Trait (Dark))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DeepShadows = DeepShadows TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepShadows :: TreacheryCard DeepShadows
deepShadows = treachery DeepShadows Cards.deepShadows

instance RunMessage DeepShadows where
  runMessage msg t@(DeepShadows attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ IfLocationExistsCalculation
          (locationWithInvestigator iid <> LocationWithTrait Dark)
          (Fixed 5)
          (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      mDetails <- runMaybeT do
        lantern <- MaybeT $ selectOne $ AssetWithTitle "Vale Lantern"
        (lantern,) <$> MaybeT (field AssetController lantern)

      chooseOrRunOneM iid $ scenarioI18n do
        unscoped $ countVar 2 $ labeled' "takeHorror" $ assignHorror iid attrs 2
        for_ mDetails \(lantern, owner) -> do
          labeled' "deepShadows.lantern" do
            flipOverBy owner attrs lantern
            connected <- select $ ConnectedFrom NotForMovement (locationWithInvestigator owner)
            chooseTargetM owner connected $ place lantern . AtLocation
      pure t
    _ -> DeepShadows <$> liftRunMessage msg attrs
