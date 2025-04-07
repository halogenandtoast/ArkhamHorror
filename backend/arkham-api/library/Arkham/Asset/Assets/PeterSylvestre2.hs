module Arkham.Asset.Assets.PeterSylvestre2 (peterSylvestre2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype PeterSylvestre2 = PeterSylvestre2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterSylvestre2 :: AssetCard PeterSylvestre2
peterSylvestre2 = ally PeterSylvestre2 Cards.peterSylvestre2 (1, 3)

instance HasModifiersFor PeterSylvestre2 where
  getModifiersFor (PeterSylvestre2 a) = controllerGets a [SkillModifier #agility 1, SkillModifier #willpower 1]

instance HasAbilities PeterSylvestre2 where
  getAbilities (PeterSylvestre2 x) =
    [ controlled x 1 (exists (HealableAsset (x.ability 1) #horror $ be x))
        $ freeReaction
        $ TurnEnds #after You
    ]

instance RunMessage PeterSylvestre2 where
  runMessage msg a@(PeterSylvestre2 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      healHorror attrs (attrs.ability 1) 1
      pure a
    _ -> PeterSylvestre2 <$> liftRunMessage msg attrs
