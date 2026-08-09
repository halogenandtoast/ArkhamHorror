module Arkham.Asset.Assets.GMen (gMen) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.GameValue (GameValue (Static))
import Arkham.Helpers.Modifiers (ModifierType (AnySkillValue), controllerGets)
import Arkham.Helpers.SkillTest (getSkillTestId)
import Arkham.Matcher
import Arkham.Placement (Placement (InPlayArea, StillInHand))

newtype GMen = GMen AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gMen :: AssetCard GMen
gMen = asset GMen Cards.gMen

instance HasModifiersFor GMen where
  getModifiersFor (GMen a) = controllerGets a [AnySkillValue 1]

instance HasAbilities GMen where
  getAbilities (GMen a) = case a.placement of
    InPlayArea _ -> [restricted a 1 ControlsThis $ forced $ PhaseEnds #when AnyPhase]
    StillInHand iid ->
      [ restricted a 2 (ControlsThis <> DuringSkillTest (SkillTestOfInvestigator $ InvestigatorWithId iid))
          $ triggered
            ( WouldHaveSkillTestResult
                #when
                (InvestigatorWithId iid)
                AnySkillTest
                (FailureResult $ EqualTo (Static 1))
            )
            Free
      ]
    _ -> []

instance RunMessage GMen where
  runMessage msg a@(GMen attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> returnToHand iid attrs >> pure a
    InHand iid (UseThisAbility iid' (isSource attrs -> True) 2) | iid == iid' -> do
      putCardIntoPlay iid attrs
      whenJustM getSkillTestId \sid -> skillTestModifier sid (attrs.ability 2) iid (AnySkillValue 1)
      pure a
    _ -> GMen <$> liftRunMessage msg attrs
