module Arkham.Location.Cards.MiramarYachtClub (miramarYachtClub) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose

newtype MiramarYachtClub = MiramarYachtClub LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miramarYachtClub :: LocationCard MiramarYachtClub
miramarYachtClub = symbolLabel $ location MiramarYachtClub Cards.miramarYachtClub 1 (PerPlayer 1)

instance HasAbilities MiramarYachtClub where
  getAbilities (MiramarYachtClub a) =
    extendRevealed1 a $ restricted a 1 Here investigateAction_

instance RunMessage MiramarYachtClub where
  runMessage msg l@(MiramarYachtClub attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigateEdit_ sid iid (attrs.ability 1) (setTarget attrs)
      pure l
    Successful (Action.Investigate, _) iid (isAbilitySource attrs 1 -> True) (isTarget attrs -> True) n -> do
      concealed <- map toId <$> getConcealedAtAll (ForExpose $ toSource attrs) attrs.id
      chooseNM iid (1 + (n `div` 2)) $ targets concealed $ revealConcealed iid (attrs.ability 1)
      pure l
    _ -> MiramarYachtClub <$> liftRunMessage msg attrs
