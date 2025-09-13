module Arkham.Location.Cards.TarPit (tarPit) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.FilmFatale.Helpers
import Arkham.Trait (Trait (Ally, Footwear))

newtype TarPit = TarPit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tarPit :: LocationCard TarPit
tarPit = location TarPit Cards.tarPit 1 (PerPlayer 3)

instance HasAbilities TarPit where
  getAbilities (TarPit a) = extendRevealed1 a $ restricted a 1 Here $ forced $ TurnEnds #when You

instance RunMessage TarPit where
  runMessage msg l@(TarPit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #combat (Fixed 2)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assets <- select (assetControlledBy iid <> hasAnyTrait [Ally, Footwear] <> DiscardableAsset)
      chooseOneM iid do
        withI18n $ countVar 1 $ labeled' "takeDirectHorror" $ directHorror iid (attrs.ability 1) 1
        scenarioI18n $ labeledValidate' (notNull assets) "tarPit.option" do
          chooseTargetM iid assets (toDiscardBy iid (attrs.ability 1))
      pure l
    _ -> TarPit <$> liftRunMessage msg attrs
