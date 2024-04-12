module Arkham.Location.Cards.PrivateRoom (privateRoom, PrivateRoom (..)) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.SkillTest.Type
import Arkham.SkillType

newtype PrivateRoom = PrivateRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

privateRoom :: LocationCard PrivateRoom
privateRoom = location PrivateRoom Cards.privateRoom 4 (Static 0)

instance HasAbilities PrivateRoom where
  getAbilities (PrivateRoom attrs) =
    withRevealedAbilities attrs [restrictedAbility attrs 1 Here parleyAction_]

instance RunMessage PrivateRoom where
  runMessage msg l@(PrivateRoom attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ parley iid (attrs.ability 1) iid #willpower (Fixed 2)
      pure l
    PassedSkillTest iid _ (isAbilitySource attrs 1 -> True) Initiator {} (SkillSkillTest sType) _ -> do
      case sType of
        SkillWillpower -> push $ parley iid (attrs.ability 1) iid #intellect (Fixed 2)
        SkillIntellect -> do
          let cost = GroupClueCost (PerPlayer 1) (LocationWithId $ toId attrs)
          canAfford <- getCanAffordCost iid (attrs.ability 1) [] [] cost
          when canAfford $ do
            player <- getPlayer iid
            investigators <- select $ investigatorAt (toId attrs)
            randolph <- getSetAsideCard Assets.randolphCarterChainedToTheWakingWorld
            push
              $ chooseOne
                player
                [ Label
                    "Pay 1{perPlayer} clues"
                    [ PayForAbility (abilityEffect attrs cost) []
                    , chooseOrRunOne
                        player
                        [targetLabel iid' [TakeControlOfSetAsideAsset iid' randolph] | iid' <- investigators]
                    ]
                , Label "Do not pay" []
                ]
        _ -> error "invalid skill type"
      pure l
    _ -> PrivateRoom <$> runMessage msg attrs
