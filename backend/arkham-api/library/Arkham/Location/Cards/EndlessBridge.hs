module Arkham.Location.Cards.EndlessBridge (endlessBridge) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Label (mkLabel)
import Arkham.Location.Cards qualified as Cards (endlessBridge)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Name
import Arkham.Scenarios.LostInTimeAndSpace.Helpers
import Control.Monad.Extra (findM)

newtype EndlessBridge = EndlessBridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endlessBridge :: LocationCard EndlessBridge
endlessBridge = location EndlessBridge Cards.endlessBridge 4 (Static 2)

instance HasAbilities EndlessBridge where
  getAbilities (EndlessBridge a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ Leaves #after Anyone (be a)

instance RunMessage EndlessBridge where
  runMessage msg l@(EndlessBridge attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      loseResources iid attrs 2
      let labels = [nameToLabel (toName attrs) <> tshow @Int n | n <- [1 .. 2]]
      availableLabel <- findM (selectNone . LocationWithLabel . mkLabel) labels
      case availableLabel of
        Just label -> pure . EndlessBridge $ attrs & labelL .~ label
        Nothing -> error "could not find label"
    UseThisAbility iid (isSource attrs -> True) 1 -> scenarioI18n do
      chooseOneM iid do
        labeled' "endlessBridge.placeDoom" $ placeDoom (attrs.ability 1) attrs 1
        labeled' "endlessBridge.discard" $ toDiscardBy iid (attrs.ability 1) attrs
      pure l
    _ -> EndlessBridge <$> liftRunMessage msg attrs
