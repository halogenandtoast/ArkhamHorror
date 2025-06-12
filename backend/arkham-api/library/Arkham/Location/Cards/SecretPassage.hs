module Arkham.Location.Cards.SecretPassage (secretPassage) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Direction
import Arkham.GameValue
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SecretPassage = SecretPassage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretPassage :: LocationCard SecretPassage
secretPassage =
  location SecretPassage Cards.secretPassage 5 (PerPlayer 1)
    & setConnectsTo (setFromList [LeftOf, RightOf])

instance HasAbilities SecretPassage where
  getAbilities (SecretPassage a) =
    extendRevealed1 a
      $ restricted a 1 (notExists $ investigatorAt a.id <> InvestigatorWithSupply Rope)
      $ forced
      $ Enters #after You (be a)

instance RunMessage SecretPassage where
  runMessage msg l@(SecretPassage attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ withI18n do
        numberVar "damage" 1
          $ numberVar "horror" 1
          $ labeled' "takeHorrorAndDamage"
          $ assignDamageAndHorror iid (attrs.ability 1) 1 1
        nameVar attrs $ countVar 1 $ labeled' "placeDoomOn" $ placeDoom (attrs.ability 1) attrs 1
      pure l
    _ -> SecretPassage <$> liftRunMessage msg attrs
