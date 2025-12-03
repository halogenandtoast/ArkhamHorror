module Arkham.Location.Cards.CatacombsOfKomElShoqafaDenOfTheBeast (catacombsOfKomElShoqafaDenOfTheBeast) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Campaigns.TheScarletKeys.Key.Types (Field (..))
import Arkham.Helpers.Cost (getCanAffordCost, payEffectCost)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenarios.DogsOfWar.Helpers
import Arkham.Token

newtype CatacombsOfKomElShoqafaDenOfTheBeast = CatacombsOfKomElShoqafaDenOfTheBeast LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catacombsOfKomElShoqafaDenOfTheBeast :: LocationCard CatacombsOfKomElShoqafaDenOfTheBeast
catacombsOfKomElShoqafaDenOfTheBeast =
  symbolLabel
    $ location
      CatacombsOfKomElShoqafaDenOfTheBeast
      Cards.catacombsOfKomElShoqafaDenOfTheBeast
      5
      (PerPlayer 2)

instance HasModifiersFor CatacombsOfKomElShoqafaDenOfTheBeast where
  getModifiersFor (CatacombsOfKomElShoqafaDenOfTheBeast a) = do
    blockedWhen a $ runValidT do
      guard a.unrevealed
      locuses <- lift keyLocusLocations
      guard $ locuses /= [a.id]

instance HasAbilities CatacombsOfKomElShoqafaDenOfTheBeast where
  getAbilities (CatacombsOfKomElShoqafaDenOfTheBeast a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage CatacombsOfKomElShoqafaDenOfTheBeast where
  runMessage msg l@(CatacombsOfKomElShoqafaDenOfTheBeast attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) attrs [#willpower, #agility] (Fixed 4)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      let cost = GroupClueCost (PerPlayer 1) (LocationWithId $ toId attrs)
      whenM (getCanAffordCost iid (attrs.ability 1) [] [] cost) do
        lightOfPharos <- selectJust $ scarletKeyIs Keys.theLightOfPharos
        chooseOneM iid $ scenarioI18n do
          labeled' "catacombsOfKomElShoqafa.pay" do
            payEffectCost iid attrs cost
            placeTokens (attrs.ability 1) lightOfPharos #resource 1
            doStep 1 msg

          labeled' "catacombsOfKomElShoqafa.doNotPay" nothing
      pure l
    DoStep 1 (PassedThisSkillTest iid (isAbilitySource attrs 1 -> True)) -> do
      investigators <- select $ investigatorAt attrs
      lightOfPharos <- selectJust $ scarletKeyIs Keys.theLightOfPharos
      tokens <- field ScarletKeyTokens lightOfPharos
      when (countTokens #resource tokens >= 3) do
        chooseTargetM iid investigators (push . PlaceScarletKey lightOfPharos . AttachedToInvestigator)
      pure l
    _ -> CatacombsOfKomElShoqafaDenOfTheBeast <$> liftRunMessage msg attrs
