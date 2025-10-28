module Arkham.Asset.Assets.AndrePatelMadeForTheSpotlight (andrePatelMadeForTheSpotlight) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype AndrePatelMadeForTheSpotlight = AndrePatelMadeForTheSpotlight AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

andrePatelMadeForTheSpotlight :: AssetCard AndrePatelMadeForTheSpotlight
andrePatelMadeForTheSpotlight = ally AndrePatelMadeForTheSpotlight Cards.andrePatelMadeForTheSpotlight (2, 2)

instance HasModifiersFor AndrePatelMadeForTheSpotlight where
  getModifiersFor (AndrePatelMadeForTheSpotlight a) = controllerGets a [SkillModifier #agility 1]

instance HasAbilities AndrePatelMadeForTheSpotlight where
  getAbilities (AndrePatelMadeForTheSpotlight a) =
    [ controlled_ a 1
        $ triggered
          (SkillTestResult #after You (SkillTestWithCommittedCards $ LengthIs (atLeast 2)) #success)
          (exhaust a)
    ]

instance RunMessage AndrePatelMadeForTheSpotlight where
  runMessage msg a@(AndrePatelMadeForTheSpotlight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      takeActionAsIfTurn iid (attrs.ability 1)
      pure a
    _ -> AndrePatelMadeForTheSpotlight <$> liftRunMessage msg attrs
