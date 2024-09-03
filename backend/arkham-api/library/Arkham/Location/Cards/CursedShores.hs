module Arkham.Location.Cards.CursedShores (CursedShores (..), cursedShores, cursedShoresEffect) where

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards (cursedShores)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype CursedShores = CursedShores LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cursedShores :: LocationCard CursedShores
cursedShores = location CursedShores Cards.cursedShores 1 (Static 0)

instance HasAbilities CursedShores where
  getAbilities (CursedShores attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility attrs 1 Here actionAbility
      , mkAbility attrs 2 $ forced $ Leaves #when You $ be attrs
      ]

instance RunMessage CursedShores where
  runMessage msg l@(CursedShores attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll
        [ assignDamage iid (attrs.ability 1) 1
        , createCardEffect Cards.cursedShores Nothing attrs iid
        ]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      skillCards <-
        fieldMap InvestigatorHand (map toCardId . filter (`cardMatch` CardWithType SkillType)) iid
      case skillCards of
        [] -> pure ()
        xs -> do
          player <- getPlayer iid
          push
            $ chooseOne
              player
              [TargetLabel (CardIdTarget x) [DiscardCard iid (attrs.ability 2) x] | x <- xs]
      pure l
    _ -> CursedShores <$> runMessage msg attrs

newtype CursedShoresEffect = CursedShoresEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cursedShoresEffect :: EffectArgs -> CursedShoresEffect
cursedShoresEffect = cardEffect CursedShoresEffect Cards.cursedShores

instance HasModifiersFor CursedShoresEffect where
  getModifiersFor target (CursedShoresEffect a) | target == a.target = do
    mSkillTestSource <- getSkillTestSource
    pure [toModifier a (AnySkillValue 2) | isJust mSkillTestSource]
  getModifiersFor _ _ = pure []

instance RunMessage CursedShoresEffect where
  runMessage msg e@(CursedShoresEffect attrs) = case msg of
    SkillTestEnds _ _ _ -> do
      push $ disable attrs
      pure e
    EndTurn iid | InvestigatorTarget iid == attrs.target -> do
      push $ disable attrs
      pure e
    _ -> CursedShoresEffect <$> runMessage msg attrs
