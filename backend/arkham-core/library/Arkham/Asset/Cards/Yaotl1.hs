module Arkham.Asset.Cards.Yaotl1
  ( yaotl1
  , Yaotl1(..)
  , yaotl1Effect
  , Yaotl1Effect(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Effect.Types
import Arkham.Effect.Runner ()
import Arkham.Investigator.Types (Field(..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Target

newtype Yaotl1 = Yaotl1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yaotl1 :: AssetCard Yaotl1
yaotl1 = ally Yaotl1 Cards.yaotl1 (2, 2)

instance HasAbilities Yaotl1 where
  getAbilities (Yaotl1 a) =
    [ withTooltip
        "{fast} Exhaust Yaotl: During this skill test, you get a bonus to each skill equal to the number of matching skill icons on the top card of your discard pile (not counting {skillWild} icons)."
      $ restrictedAbility a 1 (ControlsThis <> DuringSkillTest AnySkillTest)
      $ FastAbility
      $ ExhaustCost (toTarget a)
    , withTooltip
        "{fast}: Discard the top card of your deck. (Limit once per phase.)"
      $ limitedAbility (PlayerLimit PerPhase 1)
      $ restrictedAbility a 2 (ControlsThis <> CanManipulateDeck)
      $ FastAbility Free
    ]

instance RunMessage Yaotl1 where
  runMessage msg a@(Yaotl1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      push $ CreateEffect "04035" Nothing source (InvestigatorTarget iid)
      pure a
    UseCardAbility iid source _ 2 _ | isSource attrs source -> do
      push $ DiscardTopOfDeck iid 1 Nothing
      pure a
    _ -> Yaotl1 <$> runMessage msg attrs

newtype Yaotl1Effect = Yaotl1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yaotl1Effect :: EffectArgs -> Yaotl1Effect
yaotl1Effect = Yaotl1Effect . uncurry4 (baseAttrs "04035")

instance HasModifiersFor Yaotl1Effect where
  getModifiersFor target@(InvestigatorTarget iid) (Yaotl1Effect a) | effectTarget a == target = do
    discard <- field InvestigatorDiscard iid
    case discard of
      [] -> pure []
      (x:_) -> do
        let skillsWithCounts = mapToList $ frequencies (filter (/= SkillWild) . cdSkills $ toCardDef x)
        pure $ toModifiers a $ map (uncurry SkillModifier) skillsWithCounts
  getModifiersFor _ _ = pure []

instance RunMessage Yaotl1Effect where
  runMessage msg e@(Yaotl1Effect attrs@EffectAttrs {..}) = case msg of
    SkillTestEnds _ -> do
      push (DisableEffect effectId)
      pure e
    _ -> Yaotl1Effect <$> runMessage msg attrs
