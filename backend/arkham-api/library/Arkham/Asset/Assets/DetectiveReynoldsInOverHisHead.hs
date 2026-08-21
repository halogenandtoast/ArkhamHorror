module Arkham.Asset.Assets.DetectiveReynoldsInOverHisHead (detectiveReynoldsInOverHisHead) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (DiscoverClues)
import Arkham.ForMovement
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype DetectiveReynoldsInOverHisHead = DetectiveReynoldsInOverHisHead AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

detectiveReynoldsInOverHisHead :: AssetCard DetectiveReynoldsInOverHisHead
detectiveReynoldsInOverHisHead =
  ally DetectiveReynoldsInOverHisHead Cards.detectiveReynoldsInOverHisHead (3, 3)

instance HasModifiersFor DetectiveReynoldsInOverHisHead where
  getModifiersFor (DetectiveReynoldsInOverHisHead a) = controllerGets a [SkillModifier #intellect 1]

instance HasAbilities DetectiveReynoldsInOverHisHead where
  getAbilities (DetectiveReynoldsInOverHisHead a) =
    [ controlled a 1 (canDiscoverCluesAt $ discoverable YourLocation)
        $ triggered (DiscoverClues #after You Anywhere $ atLeast 1) (exhaust a)
    ]

discoverable :: LocationMatcher -> LocationMatcher
discoverable here = oneOf [here, RevealedLocation <> ConnectedFrom NotForMovement here]

instance RunMessage DetectiveReynoldsInOverHisHead where
  runMessage msg a@(DetectiveReynoldsInOverHisHead attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEncounterCard iid (attrs.ability 1)
      discoverAtMatchingLocation_
        iid
        (attrs.ability 1)
        (discoverable $ locationWithInvestigator iid)
        1
      pure a
    _ -> DetectiveReynoldsInOverHisHead <$> liftRunMessage msg attrs
