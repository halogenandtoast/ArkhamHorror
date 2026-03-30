module Arkham.Asset.Assets.PsychologyStudent (psychologyStudent) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype PsychologyStudent = PsychologyStudent AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

psychologyStudent :: AssetCard PsychologyStudent
psychologyStudent = ally PsychologyStudent Cards.psychologyStudent (1, 1)

instance HasAbilities PsychologyStudent where
  getAbilities (PsychologyStudent a) =
    [ controlled a 1 criteria $ freeReaction (AssetEntersPlay #after (be a))
    ]
   where
    criteria =
      oneOf
        [ exists $ HealableInvestigator (toSource a) #horror $ colocatedWithMatch You
        , exists $ HealableAsset (toSource a) #horror (#ally <> at_ YourLocation)
        ]

instance RunMessage PsychologyStudent where
  runMessage msg a@(PsychologyStudent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      investigators <- select $ HealableInvestigator source #horror (colocatedWith iid)
      assets <- select $ HealableAsset source #horror (#ally <> assetAtLocationWith iid)
      chooseOrRunOneM iid do
        targets investigators $ healHorrorOn source 2
        targets assets $ healHorrorOn source 2
      pure a
    _ -> PsychologyStudent <$> liftRunMessage msg attrs
