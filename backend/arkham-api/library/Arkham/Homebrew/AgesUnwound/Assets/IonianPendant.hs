module Arkham.Homebrew.AgesUnwound.Assets.IonianPendant (ionianPendant) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Message.SkillTest (FromSkillType (..), ToSkillType (..))

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
    [ restricted
        a
        1
        (ControlsThis <> DuringSkillTest (mapOneOf SkillTestWithSkillType [#willpower, #intellect, #agility]))
        $ triggered
          (WouldPerformRevelationSkillTest #when You)
          (exhaust a <> horrorCost a 1)
    ]

{- | The available-skill choices are built before the revelation window opens,
so adding 'UseSkillInPlaceOf' here is too late to offer combat. Choosing this
reaction performs the optional replacement directly, including its base value
and eligible commitment icons.
-}
instance RunMessage IonianPendant where
  runMessage msg a@(IonianPendant attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      for_ [#willpower, #intellect, #agility] \skill ->
        push $ ReplaceSkillTestSkill (FromSkillType skill) (ToSkillType #combat)
      pure a
    _ -> IonianPendant <$> liftRunMessage msg attrs
