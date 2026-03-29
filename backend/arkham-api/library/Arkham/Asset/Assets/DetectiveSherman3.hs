module Arkham.Asset.Assets.DetectiveSherman3 (detectiveSherman3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_, modifySelect)
import Arkham.Matcher

newtype DetectiveSherman3 = DetectiveSherman3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

detectiveSherman3 :: AssetCard DetectiveSherman3
detectiveSherman3 = ally DetectiveSherman3 Cards.detectiveSherman3 (3, 2)

instance HasModifiersFor DetectiveSherman3 where
  getModifiersFor (DetectiveSherman3 a) = for_ a.controller \iid -> do
    modified_ a iid [SkillModifier #combat 1]
    modifySelect
      a
      (not_ (InvestigatorWithId iid) <> at_ (locationWithAsset a))
      [CanAssignDamageToAsset a.id, CanAssignHorrorToAsset a.id]

instance HasAbilities DetectiveSherman3 where
  getAbilities (DetectiveSherman3 a) =
    [ controlled a 1 (AbleToDiscoverCluesAt YourLocation)
        $ triggered (AssetDealtDamage #after AnySource (be a)) (exhaust a)
    ]

instance RunMessage DetectiveSherman3 where
  runMessage msg a@(DetectiveSherman3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      pure a
    _ -> DetectiveSherman3 <$> liftRunMessage msg attrs
