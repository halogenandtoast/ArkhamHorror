module Arkham.Asset.Assets.PeterSylvestre2 (
  PeterSylvestre2 (..),
  peterSylvestre2,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Damage
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype PeterSylvestre2 = PeterSylvestre2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterSylvestre2 :: AssetCard PeterSylvestre2
peterSylvestre2 = ally PeterSylvestre2 Cards.peterSylvestre2 (1, 3)

instance HasModifiersFor PeterSylvestre2 where
  getModifiersFor (PeterSylvestre2 a) = controllerGets a [SkillModifier #agility 1, SkillModifier #willpower 1]

instance HasAbilities PeterSylvestre2 where
  getAbilities (PeterSylvestre2 x) =
    [ restrictedAbility
        x
        1
        ( ControlsThis
            <> AssetExists
              (HealableAsset (toSource x) HorrorType $ AssetWithId (toId x))
        )
        (ReactionAbility (TurnEnds Timing.After You) Free)
    ]

instance RunMessage PeterSylvestre2 where
  runMessage msg a@(PeterSylvestre2 attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ HealHorror (toTarget attrs) (toSource attrs) 1
      pure a
    _ -> PeterSylvestre2 <$> runMessage msg attrs
