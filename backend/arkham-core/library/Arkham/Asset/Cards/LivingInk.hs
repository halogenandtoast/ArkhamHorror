module Arkham.Asset.Cards.LivingInk (livingInk, LivingInk (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Helpers.Customization
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Matcher

newtype LivingInk = LivingInk AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingInk :: AssetCard LivingInk
livingInk = assetWith LivingInk Cards.livingInk (whenNoUsesL ?~ DiscardWhenNoUses)

instance HasModifiersFor LivingInk where
  getModifiersFor (InvestigatorTarget iid) (LivingInk a) = do
    maybeModified a do
      guard $ a `controlledBy` iid
      guard $ getAssetMetaDefault True a -- check subtle depiction
      -- handles EldritchInk && EldritchInk2
      let skills = [s | ChosenSkill s <- concatMap snd (toList a.customizations)]
      pure
        $ if a `hasCustomization` Vibrancy
          then
            map (`SkillModifier` 2) skills <> [SkillModifier s (-1) | s <- [minBound ..], s `notElem` skills]
          else map (`SkillModifier` 1) skills
  getModifiersFor target (LivingInk a) | a `is` target = do
    maybeModified a do
      guard $ a `hasCustomization` ImbuedInk
      pure [AdditionalStartingUses 2, DoNotTakeUpSlot #body, AdditionalSlot #arcane]
  getModifiersFor _ _ = pure []

instance HasAbilities LivingInk where
  getAbilities (LivingInk a) =
    [ restrictedAbility a 1 ControlsThis
      $ ReactionAbility (RevealChaosToken #after You IsSymbol) (exhaust a)
    | a `hasCustomization` MacabreDepiction
    ]

instance RunMessage LivingInk where
  runMessage msg a@(LivingInk attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs Charge 1
      pure a
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      when (attrs `hasCustomization` ShiftingInk) do
        iids <- select $ affectsOthers $ colocatedWith iid
        chooseOrRunOne iid $ targetLabels iids $ only . (`TakeControlOfAsset` (toId attrs))
      LivingInk <$> liftRunMessage msg attrs
    BeginTurn iid | attrs `controlledBy` iid -> do
      if attrs `hasCustomization` SubtleDepiction
        then do
          chooseOne
            iid
            [ Label
                "Do not charge from Living Ink and ignore its ability for the remainder of the round (Subtle Depiction)"
                []
            , Label "Remove 1 charge" [RemoveTokens (toSource attrs) (toTarget attrs) Charge 1]
            ]
          pure . LivingInk $ setMeta False attrs
        else
          liftRunMessage
            (RemoveTokens (toSource attrs) (toTarget attrs) Charge 1)
            (LivingInk $ setMeta False attrs)
    RemoveTokens (isSource attrs -> True) (isTarget attrs -> True) Charge 1 -> do
      attrs' <- liftRunMessage msg attrs
      pure . LivingInk $ setMeta True attrs'
    EndRound -> do
      pure . LivingInk $ setMeta True attrs
    _ -> LivingInk <$> liftRunMessage msg attrs
