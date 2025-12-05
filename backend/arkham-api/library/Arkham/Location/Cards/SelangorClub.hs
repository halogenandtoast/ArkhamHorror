module Arkham.Location.Cards.SelangorClub (selangorClub) where

import Arkham.Ability
import Arkham.Damage
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SelangorClub = SelangorClub LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

selangorClub :: LocationCard SelangorClub
selangorClub = symbolLabel $ location SelangorClub Cards.selangorClub 2 (PerPlayer 1)

instance HasModifiersFor SelangorClub where
  getModifiersFor (SelangorClub a) = modifySelect a (investigatorAt a) [DoNotCollectResourcesDuringUpkeep]

healableAsset :: Sourceable source => source -> DamageType -> LocationMatcher -> AssetMatcher
healableAsset (toSource -> source) hType loc =
  HealableAsset source hType $ at_ loc <> AssetControlledBy (affectsOthers Anyone)

instance HasAbilities SelangorClub where
  getAbilities (SelangorClub a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 (Here <> criteria)
      $ parleyAction (ResourceCost 2)
   where
    healable hType = HealableInvestigator (toSource a) hType $ at_ YourLocation
    criteria =
      oneOf
        [ exists $ oneOf $ map healable [#horror, #damage]
        , exists $ oneOf [healableAsset a kind YourLocation | kind <- [#damage, #horror]]
        ]

instance RunMessage SelangorClub where
  runMessage msg l@(SelangorClub attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1

      assets <-
        select
          $ oneOf [healableAsset source kind (locationWithInvestigator iid) | kind <- [#horror, #damage]]

      investigators <-
        select $ oneOf [HealableInvestigator source kind $ colocatedWith iid | kind <- [#horror, #damage]]
      chooseOneM iid do
        targets investigators \i -> do
          healDamage i source 1
          healHorror i source 1

        targets assets \asset' -> do
          healDamage asset' source 1
          healHorror asset' source 1
      pure l
    _ -> SelangorClub <$> liftRunMessage msg attrs
