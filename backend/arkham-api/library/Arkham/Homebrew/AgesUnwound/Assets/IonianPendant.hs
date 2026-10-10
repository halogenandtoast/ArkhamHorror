module Arkham.Homebrew.AgesUnwound.Assets.IonianPendant (ionianPendant) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype IonianPendant = IonianPendant AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Sanity 4, which the def cannot carry.
ionianPendant :: AssetCard IonianPendant
ionianPendant = assetWith IonianPendant Cards.ionianPendant (sanityL ?~ 4)

{- | "[reaction] When you would perform a skill test during a revelation effect,
exhaust Ionian Pendant and place 1 horror on it: You may use [combat] in place of
the listed skill for that test."
-}
instance HasAbilities IonianPendant where
  getAbilities (IonianPendant a) =
    [ restricted a 1 ControlsThis
        $ triggered
          (WouldPerformRevelationSkillTest #when You)
          (exhaust a <> horrorCost a 1)
    ]

{- | "In place of the listed skill" is 'UseSkillInPlaceOf', one per skill combat
could stand in for -- the modifier is keyed on the skill being replaced, and it
offers the swap rather than forcing it, which is the card's "you may".
-}
instance RunMessage IonianPendant where
  runMessage msg a@(IonianPendant attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getRevelationSkillTest -> (iid, sid)) _ -> do
      skillTestModifiers sid (attrs.ability 1) iid
        $ map (`UseSkillInPlaceOf` #combat) [#willpower, #intellect, #agility]
      pure a
    _ -> IonianPendant <$> liftRunMessage msg attrs

getRevelationSkillTest :: [Window] -> (InvestigatorId, SkillTestId)
getRevelationSkillTest [] = error "Invalid call"
getRevelationSkillTest ((windowType -> Window.WouldPerformRevelationSkillTest iid sid) : _) = (iid, sid)
getRevelationSkillTest (_ : xs) = getRevelationSkillTest xs
