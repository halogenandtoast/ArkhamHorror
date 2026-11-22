module Arkham.Asset.Assets.StaffOfAtal3 (staffOfAtal3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Dreaming (dreamPlaceDreams, getDreamPlaces)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Token

newtype StaffOfAtal3 = StaffOfAtal3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

staffOfAtal3 :: AssetCard StaffOfAtal3
staffOfAtal3 = asset StaffOfAtal3 Cards.staffOfAtal3

instance HasModifiersFor StaffOfAtal3 where
  getModifiersFor (StaffOfAtal3 a) =
    for_ a.controller \iid -> modified_ a iid [AdditionalDreamingCard 1]

{- | The @Who@ comes from the flip's own payload, which is the investigator whose
dream zone receives the card -- that is, the asset's controller. Not an
investigator matcher re-derived from the asset, which has already left play.
-}
instance HasAbilities StaffOfAtal3 where
  getAbilities (StaffOfAtal3 a) =
    [ controlled_ a 1
        $ triggered (AssetFlippedFacedown #after You (AssetControlledBy You)) (exhaust a)
    ]

instance RunMessage StaffOfAtal3 where
  runMessage msg a@(StaffOfAtal3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      -- dream PLACES, so Idol of Bokrug is a legal source like any dreaming card
      sources <- filter ((> 0) . dreamPlaceDreams) <$> getDreamPlaces iid
      -- "and", not "you may": with no dream to move the clue still happens
      when (notNull sources) do
        chooseOrRunOneM iid $ targets sources \from ->
          moveTokens (attrs.ability 1) from (ResourceTarget iid) Dream 1
      pure a
    _ -> StaffOfAtal3 <$> liftRunMessage msg attrs
