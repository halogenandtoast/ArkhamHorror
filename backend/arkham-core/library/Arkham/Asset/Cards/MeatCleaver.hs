module Arkham.Asset.Cards.MeatCleaver (meatCleaver, meatCleaverEffect, MeatCleaver (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Runner
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
    UseCardAbility iid source 1 _ payments | isSource attrs source -> do
      remainingSanity <- field InvestigatorRemainingSanity iid
      pushAll
        [ skillTestModifiers attrs iid
            $ [SkillModifier #combat (if remainingSanity <= 3 then 2 else 1)]
            <> [DamageDealt 1 | paidHorror payments]
        , createCardEffect Cards.meatCleaver Nothing source iid
        , chooseFightEnemy iid source #combat
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
    EnemyDefeated _ _ source _ | effectSource attrs == source -> do
      case effectTarget attrs of
        InvestigatorTarget iid -> do
          mHealHorror <- getHealHorrorMessage source 1 iid
          pushAll
            $ maybeToList mHealHorror
            <> [DisableEffect $ toId attrs]
          pure e
        _ -> error "Invalid target"
    SkillTestEnds _ _ -> e <$ push (DisableEffect $ toId attrs)
    _ -> MeatCleaverEffect <$> runMessage msg attrs
