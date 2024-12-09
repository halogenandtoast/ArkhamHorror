module Arkham.Asset.Assets.HikingBoots1 (hikingBoots1, HikingBoots1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher
import Arkham.Movement

newtype HikingBoots1 = HikingBoots1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hikingBoots1 :: AssetCard HikingBoots1
hikingBoots1 = asset HikingBoots1 Cards.hikingBoots1

instance HasModifiersFor HikingBoots1 where
  getModifiersFor (HikingBoots1 a) = controllerGets a [SkillModifier #agility 1]

instance HasAbilities HikingBoots1 where
  getAbilities (HikingBoots1 a) =
    [ controlledAbility
        a
        1
        ( exists
            $ CanMoveToLocation You (a.ability 1)
            $ ConnectedLocation
            <> oneOf [LocationWithAnyClues, UnrevealedLocation]
        )
        $ ReactionAbility (DiscoveringLastClue #after You YourLocation) (exhaust a)
    ]

instance RunMessage HikingBoots1 where
  runMessage msg a@(HikingBoots1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <-
        select
          $ CanMoveToLocation (InvestigatorWithId iid) (attrs.ability 1)
          $ ConnectedLocation
          <> oneOf [LocationWithAnyClues, UnrevealedLocation]
      chooseOrRunOne
        iid
        [targetLabel location [Move $ move (attrs.ability 1) iid location] | location <- locations]
      pure a
    _ -> HikingBoots1 <$> liftRunMessage msg attrs
