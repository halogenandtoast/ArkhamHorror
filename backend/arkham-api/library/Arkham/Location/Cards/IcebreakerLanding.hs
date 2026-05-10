module Arkham.Location.Cards.IcebreakerLanding (icebreakerLanding, IcebreakerLanding (..)) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Supplies
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.IceAndDeath.Helpers

newtype IcebreakerLanding = IcebreakerLanding LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

icebreakerLanding :: LocationCard IcebreakerLanding
icebreakerLanding =
  symbolLabel
    $ locationWith IcebreakerLanding Cards.icebreakerLanding 2 (PerPlayer 2)
    $ (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 2) YourLocation)

instance HasAbilities IcebreakerLanding where
  getAbilities (IcebreakerLanding a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here parleyAction_

instance RunMessage IcebreakerLanding where
  runMessage msg l@(IcebreakerLanding attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) iid #intellect (Fixed 4)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      oncePerCampaign "icebreakerLanding" do
        chooseOneM iid $ cardI18n $ scope "icebreakerLanding" do
          whenAny (ChaosTokenFaceIs #frost) do
            labeled' "removeFrost" $ removeChaosToken #frost
          labeled' "recordSmallRadio"
            $ recoverSupply SmallRadio

      pure l
    _ -> IcebreakerLanding <$> liftRunMessage msg attrs
