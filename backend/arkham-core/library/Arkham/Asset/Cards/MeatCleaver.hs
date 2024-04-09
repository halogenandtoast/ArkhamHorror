module Arkham.Asset.Cards.MeatCleaver (meatCleaver, meatCleaverEffect, MeatCleaver (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Runner
import Arkham.Fight
import Arkham.Helpers.Investigator
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection

newtype MeatCleaver = MeatCleaver AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

meatCleaver :: AssetCard MeatCleaver
meatCleaver = asset MeatCleaver Cards.meatCleaver

instance HasAbilities MeatCleaver where
  getAbilities (MeatCleaver attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ fightAction (UpTo 1 $ HorrorCost (toSource attrs) YouTarget 1)
    ]

paidHorror :: Payment -> Bool
paidHorror (HorrorPayment _) = True
paidHorror (Payments ps) = any paidHorror ps
paidHorror _ = False

instance RunMessage MeatCleaver where
  runMessage msg a@(MeatCleaver attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ payments -> do
      let source = attrs.ability 1
      remainingSanity <- field InvestigatorRemainingSanity iid
      let n = if remainingSanity <= 3 then 2 else 1
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll
        [ skillTestModifiers attrs iid $ SkillModifier #combat n : [DamageDealt 1 | paidHorror payments]
        , createCardEffect Cards.meatCleaver Nothing source iid
        , chooseFight
        ]
      pure a
    _ -> MeatCleaver <$> runMessage msg attrs

newtype MeatCleaverEffect = MeatCleaverEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

meatCleaverEffect :: EffectArgs -> MeatCleaverEffect
meatCleaverEffect = cardEffect MeatCleaverEffect Cards.meatCleaver

instance RunMessage MeatCleaverEffect where
  runMessage msg e@(MeatCleaverEffect attrs) = case msg of
    EnemyDefeated _ _ source _ | attrs.source == source -> do
      case attrs.target of
        InvestigatorTarget iid -> do
          canBeHealed <- canHaveHorrorHealed attrs.source iid
          pushAll $ [HealHorror (toTarget iid) attrs.source 1 | canBeHealed] <> [disable attrs]
        _ -> error "Invalid target"
      pure e
    SkillTestEnds _ _ -> e <$ push (disable attrs)
    _ -> MeatCleaverEffect <$> runMessage msg attrs
