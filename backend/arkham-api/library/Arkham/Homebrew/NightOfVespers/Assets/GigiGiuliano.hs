module Arkham.Homebrew.NightOfVespers.Assets.GigiGiuliano (gigiGiuliano) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers hiding (skillTestModifier)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.NightOfVespers.CardDefs.Assets qualified as Cards
import Arkham.Matcher

newtype GigiGiuliano = GigiGiuliano AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gigiGiuliano :: AssetCard GigiGiuliano
gigiGiuliano = ally GigiGiuliano Cards.gigiGiuliano (2, 2)

instance HasModifiersFor GigiGiuliano where
  getModifiersFor (GigiGiuliano a) = controllerGets a [SkillModifier #agility 1]

instance HasAbilities GigiGiuliano where
  getAbilities (GigiGiuliano a) =
    [ playerLimit PerRound
        $ controlled_ a 1
        $ triggered
          (InitiatedSkillTest #when You (IsSkillType #willpower) #any (SkillTestOnTreachery AnyTreachery))
          (exhaust a)
    ]

instance RunMessage GigiGiuliano where
  runMessage msg a@(GigiGiuliano attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) sid (Difficulty (-2))
      pure a
    _ -> GigiGiuliano <$> liftRunMessage msg attrs
