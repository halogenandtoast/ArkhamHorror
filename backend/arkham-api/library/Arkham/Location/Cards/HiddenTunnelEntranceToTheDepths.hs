module Arkham.Location.Cards.HiddenTunnelEntranceToTheDepths (hiddenTunnelEntranceToTheDepths) where

import Arkham.Ability
import Arkham.Helpers.Modifiers hiding (skillTestModifier)
import Arkham.Investigator.Types (Field (..))
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype HiddenTunnelEntranceToTheDepths = HiddenTunnelEntranceToTheDepths LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenTunnelEntranceToTheDepths :: LocationCard HiddenTunnelEntranceToTheDepths
hiddenTunnelEntranceToTheDepths =
  locationWith
    HiddenTunnelEntranceToTheDepths
    Cards.hiddenTunnelEntranceToTheDepths
    0
    (PerPlayer 2)
    ((shroudL .~ Nothing) . connectsToAdjacent)

instance HasModifiersFor HiddenTunnelEntranceToTheDepths where
  getModifiersFor (HiddenTunnelEntranceToTheDepths a) = do
    whenUnrevealed a $ modifySelf a [Blocked]
    modifySelf a [CannotMoveCluesFromHere]
    modifySelect a Anyone [CannotDiscoverCluesAt (be a)]

instance HasAbilities HiddenTunnelEntranceToTheDepths where
  getAbilities (HiddenTunnelEntranceToTheDepths a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage HiddenTunnelEntranceToTheDepths where
  runMessage msg l@(HiddenTunnelEntranceToTheDepths attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skullKeys <-
        filter (maybe False ((== #skull) . (.face)) . preview _TokenKey)
          . toList
          <$> field InvestigatorKeys iid
      case nonEmpty skullKeys of
        Just (k :| _) -> do
          chooseOneM iid do
            labeled "Spend Skull Key" do
              push $ PlaceKey ScenarioTarget k
              doStep 2 msg
            labeled "Do not Spend Skull Key" $ doStep 1 msg
        Nothing -> doStep 1 msg
      pure l
    DoStep n (UseThisAbility iid (isSource attrs -> True) 1) -> do
      sid <- getRandom
      when (n == 2) do
        skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 5)
      chooseOneM iid do
        for_ [minBound ..] \kind ->
          skillLabeled kind $ beginSkillTest sid iid (attrs.ability 1) iid kind (Fixed 4)
      pure l
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      removeTokens (attrs.ability 1) attrs #clue 1
      pure l
    PassedThisSkillTest _iid (isAbilitySource attrs 2 -> True) -> do
      removeTokens (attrs.ability 1) attrs #clue 2
      pure l
    _ -> HiddenTunnelEntranceToTheDepths <$> liftRunMessage msg attrs
