module Arkham.Asset.Cards.Yaotl1 (yaotl1, Yaotl1 (..), yaotl1Effect, Yaotl1Effect (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Effect.Import
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection
import Arkham.SkillType

newtype Yaotl1 = Yaotl1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yaotl1 :: AssetCard Yaotl1
yaotl1 = ally Yaotl1 Cards.yaotl1 (2, 2)

instance HasAbilities Yaotl1 where
  getAbilities (Yaotl1 a) =
    [ withTooltip
        "{fast} Exhaust Yaotl: During this skill test, you get a bonus to each skill equal to the number of matching skill icons on the top card of your discard pile (not counting {skillWild} icons)."
        $ controlledAbility a 1 DuringAnySkillTest
        $ FastAbility
        $ exhaust a
    , withTooltip "{fast}: Discard the top card of your deck. (Limit once per phase.)"
        $ playerLimit PerPhase
        $ controlledAbility a 2 CanManipulateDeck
        $ FastAbility Free
    ]

instance RunMessage Yaotl1 where
  runMessage msg a@(Yaotl1 attrs) = case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        push $ createCardEffect Cards.yaotl1 Nothing (attrs.ability 1) sid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ DiscardTopOfDeck iid 1 (toAbilitySource attrs 2) Nothing
      pure a
    _ -> Yaotl1 <$> runMessage msg attrs

newtype Yaotl1Effect = Yaotl1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yaotl1Effect :: EffectArgs -> Yaotl1Effect
yaotl1Effect = cardEffect Yaotl1Effect Cards.yaotl1

instance HasModifiersFor Yaotl1Effect where
  getModifiersFor (InvestigatorTarget iid) (Yaotl1Effect a) = maybeModified a do
    sid <- MaybeT getSkillTestId
    guard $ isTarget sid a.target
    iid' <- MaybeT getSkillTestInvestigator
    guard $ iid == iid'
    discard <- lift $ field InvestigatorDiscard iid
    case discard of
      [] -> pure []
      (x : _) -> do
        let
          skillIcons = cdSkills $ toCardDef x
          skillCount sk = count (== SkillIcon sk) skillIcons
        pure
          [ SkillModifier sk n
          | sk <- allSkills
          , let n = skillCount sk
          , n > 0
          ]
  getModifiersFor _ _ = pure []

instance RunMessage Yaotl1Effect where
  runMessage msg e@(Yaotl1Effect attrs) = case msg of
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> do
      push (DisableEffect attrs.id)
      pure e
    _ -> Yaotl1Effect <$> runMessage msg attrs
